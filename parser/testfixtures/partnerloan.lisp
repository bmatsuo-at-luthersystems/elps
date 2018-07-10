(in-package 'partnerloan)
(use-package 'router)

; Because the partnerloan phylum deals with large numeric inputs it we have the
; json package deserialize numbers as lisp strings (and serialize lisp numbers
; as JSON strings) by default.  All accesses on numberic map fields should
; perform an appropriate coersion (e.g. to-int, to-float) before working with
; numeric data.
(json:use-string-numbers true)

; chaincode metadata
(set 'version "18.06.20")
(set 'service-name "partnerloan01")

; default-max-facilities is an inclusive upper limit on the number of enabled
; facilities.
(set 'default-max-facilities 3)

;; Generic helper functions

(defun string-in? (s lis)
  (any? (expr (string= % s)) lis))

(defun count (predicate seq)
  (foldl + 0 (map 'list
                  (lambda (x) (if (predicate x) 1 0))
                  seq)))

;; Helper functions for interacting with storage

(defun storage-put-json (key val)
  (cc:storage-put key (json:dump-bytes val)))

(defun storage-get-json (key)
  (let ([data (cc:storage-get key)])
    (ignore-errors (json:load-bytes data))))

; Helper functions for interacting with maps
(defun get-required (obj key)
  (if (key? obj key)
    (get obj key)
    (error 'map-missing-key
           (format-string "Missing required key {}" key))))

(defmacro get-nested-bool (obj &rest keys)
  (quasiquote (get-nested-default (unquote obj)
                                  false
                                  (unquote-splicing keys))))

(defmacro get-nested-string (obj &rest keys)
  (quasiquote (get-nested-default (unquote obj)
                                  ""
                                  (unquote-splicing keys))))

(defmacro get-nested-number (obj &rest keys)
  (quasiquote (get-nested-default (unquote obj)
                                  0
                                  (unquote-splicing keys))))

(defmacro get-nested-vector (obj &rest keys)
  (quasiquote (get-nested-default (unquote obj)
                                  (vector)
                                  (unquote-splicing keys))))

(defmacro get-nested-map (obj &rest keys)
  (quasiquote (get-nested-default (unquote obj)
                                  (sorted-map)
                                  (unquote-splicing keys))))

(defmacro get-nested-default (obj default &rest keys)
  (if (= 0 (length keys))
    obj
    (let ([ksym (gensym)])
      (quasiquote
        (let ([(unquote ksym) (unquote (car keys))])
          (if (not (key? (unquote obj) (unquote ksym)))
            (unquote default)
            (partnerloan:get-nested-default (get (unquote obj)
                                                 (unquote ksym))
                                            (unquote default)
                                            (unquote-splicing (cdr keys)))))))))

(defun get-nested (obj &rest keys)
  (if (nil? keys)
    obj
    (let ([k (first keys)])
      (if (not (key? obj k))
        ()
        (get-nested (get obj k) (cdr keys))))))

(defun values (type-specifier obj) 
  (map type-specifier (get obj) (keys obj)))

;; Enum utilities

(defun valid-currency? (c)
  (any? (expr (string= c %))
        valid-currencies))
(set 'valid-currencies '("GBP" "EUR"))

(defun valid-role? (r)
  (any? (expr (string= r %))
        valid-currencies))
(set 'valid-roles '(
                    "FINANCE"    ; DLA Finance Staff
                    "PARTNER"    ; DLA Partner
                    "EXECUTIVE"  ; DLA Executive
                    "BANK"       ; External bank role.
))

;; Facility model functions

(defun num-facilities-enabled (facilities) 
  (length (select 'list facility-enabled? (values 'list facilities))))

(defun facility-enabled? (facility)
  (get-default facility "enabled" false))

(defun storage-put-facilities (facilities)
  (storage-put-json "partnerloan:facilities" facilities))

(defun storage-get-facilities ()
  (storage-get-json "partnerloan:facilities"))

(defun make-storage-key (&rest identifier)
  (string:join identifier ":"))

;; Person model functions

(defun person-id-from-key (storage-key)
  (nth (string:split storage-key ":") 1))

(defun storage-get-person (id &optional include-events?)
  (let ([person (storage-get-json (make-storage-key "person" id))])
    (if (not include-events?)
      person
      (assoc! person "event_history" (storage-get-person-events id)))))

(defun get-person-id-from-email (email)
  (storage-get-json (make-storage-key "person_id_from_email" email)))
   
(defun storage-put-person (person)
  (let* ([id    (get-required person "person_id")]
         [email (get-required person "email")])
    (storage-put-json (make-storage-key "person" id)
                      person)
    (storage-put-json (make-storage-key "person_id_from_email" email)
                      id)))

(defun storage-get-person-events (person-id)
  (let* ([events-key  (make-storage-key "person_events" person-id)])
    (or (storage-get-json events-key)
        (vector))))
   
(defun storage-put-person-event (person-id event)
  (let* ([events-key  (make-storage-key "person_events" person-id)]
         [events      (or (storage-get-json events-key)
                          (vector))]
         ; NOTE:  This assumes events are added in sorted (chronological) order.
         [new-events  (concat 'vector events (list event))])
    (storage-put-json events-key new-events)))

(defun implicit-partner-from-email (partner-email &key partner-name partner-surname)
  ; If email already maps to a person we just return that, otherwise we want to
  ; create a new partner record in storage which can be returned.  If the
  ; specified person exists and is not PARTNER then nil is returned.
  ;
  ; NOTE:  If a person already exists the keyword arguments partner-name and
  ; partner-surname will be ignored, not be checked for consistency with the
  ; stored record.
  (let* ([partner-id          (get-person-id-from-email partner-email)]
         [partner             (if partner-id
                                (storage-get-person partner-id true)
                                ; TODO:   Trigger an email notification (and
                                ; external account creation) when creating a
                                ; new partner.
                                (let ([person (sorted-map "person_id" (cc:make-id)
                                                          "name"      (or partner-name "")
                                                          "surname"   (or partner-surname "")
                                                          "email"     partner-email
                                                          "position"  ""
                                                          "roles"     ["PARTNER"])])
                                  (storage-put-person person)
                                  person))])
    (if (person-has-any-role? partner ["PARTNER"])
      partner
      ())))

(defun find-people (&optional ids include-events?)
  (if (< 0 (length ids))
    (map 'list
         (lambda (id) (and id (storage-get-person id include-events?)))
         ids)
    (let ([fun (lambda (lis key _)
                 ; This is so inefficient it hurts.
                 (concat 'list lis (list (storage-get-person (person-id-from-key key) include-events?))))])
      (cc:storage-reduce-range fun '()
                               "person:"
                               "person:~"))))

(defun person-has-any-role? (p roles)
  (let ([person-roles (get-nested-vector p "roles")])
    (any? (expr (string-in? % person-roles))
          roles)))

(defun executive-id? (id)
  (let ([person (storage-get-person id)])
    (any? (expr (string= % "EXECUTIVE")) (get-nested-vector person "roles"))))

(defun default-people ()
  (let ([lutherAdminRoles (vector "PARTNER" "FINANCE" "EXECUTIVE" "BANK")])
    (list
      (sorted-map "person_id" "person0"
                  "name"      "Jane"
                  "surname"   "Smith"
                  "email"     "partnerloan-example+jane@luthersystems.com"
                  "roles"     (vector "PARTNER" "FINANCE"))
      (sorted-map "person_id" "person1"
                  "name"      "Susan"
                  "surname"   "Thorpe"
                  "email"     "partnerloan-example+susan@luthersystems.com"
                  "roles"     (vector "PARTNER" "EXECUTIVE"))
      (sorted-map "person_id" "person2"
                  "name"      "Jonah"
                  "surname"   "Bankman"
                  "email"     "partnerloan-example+jonah@luthersystems.com"
                  "roles"     (vector "BANK"))
      )))

; Loan model functions
(defun make-storage-loan-key (id)
  (let ([now (cc:timestamp-unix-reverse (cc:now))])
    (make-storage-key "loan" now id)))

(defun loan-id-from-key (key)
  (nth (string:split key ":") 2))

(defun storage-get-loan (id)
  (let ([events (storage-get-json (make-storage-key "loan_events" id))])
    (sorted-map "loan_id"  id
                "event_history" events)))

(defun storage-update-loan (loan)
  (let* ([id (get-required loan "loan_id")]
         [events (get-nested-vector loan "event_history")])
    (storage-put-json (make-storage-key "loan_events" id)
                      events)))

(defun storage-put-loan (loan)
  (let* ([id  (get-required loan "loan_id")]
         [key (make-storage-loan-key id)])
    (storage-update-loan loan)
    (storage-put-json key (vector))))

(defun find-loans (&optional ids)
  (if (< 0 (length ids))
    (map 'list storage-get-loan ids)
    (let ([fun (lambda (lis key data)
                 ; This is so inefficient it hurts.
                 (concat 'list lis (list (storage-get-loan (loan-id-from-key key)))))])
      (cc:storage-reduce-range fun '()
                               "loan:"
                               "loan:~"))))
; Routes
(defendpoint "init" ()
  (cc:set-tx-metadata "txEventName" "Chaincode Initialized")
  (let* ([prev-version  (storage-get-json "partnerloan:version")]
         [init?         (nil? prev-version)]
         [facilities    (sorted-map)])
    (if init?
      (progn
        (map () storage-put-person (default-people))
        (storage-put-json "partnerloan:facilities" facilities)
        (cc:infof (sorted-map)
                  "Chaincode initialized with version: {}" version))
      (cc:infof (sorted-map)
                "Chaincode upgraded from version: {} to version: {}"
                prev-version
                version))
    (storage-put-json "partnerloan:version" version)
    (route-success ())))

(defendpoint "healthcheck" ()
  (route-success
    (sorted-map "reports"
      (vector (sorted-map
                "status"          "UP"
                "service_version" version
                "service_name"    service-name
                "timestamp"       (cc:timestamp (cc:now)))))))

(defendpoint "get_facilities" (obj)
  (cc:set-tx-metadata "txEventName" "Get Facilities")
  (let* ([facilities       (storage-get-facilities)]
         [all-ids          (keys facilities)]
         [only-ids?        (get-default obj "facility_ids_only" false)]
         [all-enabled-ids  (select 'list
                                   (lambda(id)
                                     (thread-last
                                       (get facilities id)
                                       (facility-enabled?)))
                                   all-ids)]
         [req-ids (get-nested-vector obj "facility_ids")]
         [resp-ids (if (= 0 (length req-ids))
                      all-enabled-ids
                      req-ids)]
         [facilities (map 'vector
                          (lambda (id)
                            (if only-ids?
                              (sorted-map "facility_id" id)
                              (get facilities id)))
                          resp-ids)]
         [response (sorted-map "facilities" facilities)])
    (route-success response)))

(defendpoint "create_facility" (obj)
  (cc:set-tx-metadata "txEventName" "Create Facility")
  (let* ([facilities (or (storage-get-facilities) (sorted-map))]
         [id (cc:make-id)]
         [facility (sorted-map
                     "facility_id"       id
                     "name"              (get-nested-string obj "name")
                     "currency"          (get-nested-string obj "currency")
                     "max_amount"        (get-nested-number obj "max_amount")
                     "available_amount"  (get-nested-number obj "available_amount")
                     "enabled"           true)])
    ; TODO: check currency
    ; TODO: bounds check amounts
    (assoc! facilities id facility)
    (if (> (num-facilities-enabled facilities) default-max-facilities)
      (route-failure "Max facilities exceeded")
      (progn
        (storage-put-facilities facilities)
        (route-success facility)))))

(defendpoint "update_facility" (obj)
  (cc:set-tx-metadata "txEventName" "Update Facility")
  (let* ([facilities    (storage-get-facilities)]
         [id            (get-nested-string obj "facility_id")]
         [orig-facility (get facilities id)]
         [new-facility  (sorted-map
                          "facility_id"       id
                          "name"              (get-nested-string obj "name")
                          "currency"          (get-nested-string obj "currency")
                          "max_amount"        (get-nested-number obj "max_amount")
                          "available_amount"  (get-nested-number obj "available_amount")
                          "enabled"           (get-nested-bool obj "enabled"))])
    ; TODO: check currency against prev
    ; TODO: bounds check amounts
    (assoc! facilities id new-facility)
    (if (> (num-facilities-enabled facilities) default-max-facilities)
      (route-failure "Max facilities exceeded")
      (progn
        (storage-put-facilities facilities)
        (route-success new-facility)))))

; NOTE:  create_person does not accept person_id or event_history fields of the
; person.  Also, the "name" of a person is not required in order to allow
; implicit registration of admin users.
(defschema "create_person" ["""
{
  "type": "object",
  "description": "Create a new person on the Blockchain.",
  "required": ["person"],
  "properties": {
    "person": {
      "type": "object",
      "description": "Person details.",
      "required": ["email","position","roles"],
      "properties": {
        "name": {
          "type": "string",
          "description": "Name of the person."
        },
        "surname": {
          "type": "string",
          "description": "Surname of the person."
        },
        "email": {
          "type": "string",
          "description": "Email of the person.",
          "format": "email"
        },
        "position": {
          "type": "string",
          "description": "The human readable positon name within DLA piper."
        },
        "roles": {
          "type": "array",
          "description": "Their roles.",
          "uniqueItems": true,
          "minItems": 1,
          "items": {
            "type": "string",
            "enum": [
              "FINANCE",
              "PARTNER",
              "EXECUTIVE",
              "BANK"
            ]
          }
        }
      }
    }
  }
}
"""])
(defendpoint "create_person" (obj)
  (cc:set-tx-metadata "txEventName" "Create Person")
  (let* ([person (get-nested-map obj "person")]
         [email (get-nested-string person "email")]
         [person-id (cc:make-id)])
    (assoc! person "person_id" person-id)
    (cond
      ((= 0 (length email))
       (route-failure "Invalid email"))
      ((get-person-id-from-email email)
       (route-failure "Invalid request"))
      (:else
       (storage-put-person person)
       (route-success (sorted-map "person" person))))))

(defendpoint "get_persons" (obj)
  (cc:set-tx-metadata "txEventName" "Get Person")
  (let* ([ids-only?  (get-nested-bool obj "person_ids_only")]
         [summary    (get-nested-bool obj "summary")]  ;   When true, events are not included
         [ids        (get-nested-vector obj "person_ids")]
         [emails     (get-nested-vector obj "person_emails")]
         [email-ids  (map 'vector get-person-id-from-email emails)]
         [roles      (get-nested-vector obj "filter_roles")])
    (cond
      ; Validate inputs
      ((not (all? string? ids))
       (route-failure "Invalid person_ids"))
      ((not (all? string? emails))
       (route-failure "Invalid person_emails"))
      ((not (all? string? roles))
       (route-failure "Invalid filter_roles"))
      ((< 1 (count (lambda (lis) (< 0 (length lis)))
                   (list ids emails roles)))
       (route-failure "Only one of these parameters may be provided: person_ids, person_emails, roles"))
      (:else
        ; Get person objects and filter the results
        (let* ([ids     (if (= 0 (length emails)) ids email-ids)]
               [people  (find-people ids (not summary))]
               ; if find-people returns nil values we need to turn those into
               ; empty objects for eventual protobuf serialization.
               [people  (map 'list
                             (lambda (p) (or p (sorted-map)))
                             people)]
               ;  Filter the list of people whose "roles" list intersects with
               ;  the given roles (if any were provided).
               [people  (if (= 0 (length roles))
                          people
                          (select 'list
                                  (lambda (p) (person-has-any-role? p roles))
                                  people)) ]
               ;  "Strip" unwanted fields by creating a copy of the person.
               [people  (if (not ids-only?)
                          people
                          (map 'list (lambda (p)
                                       (sorted-map
                                         "person_id" (get p "person_id")))
                               people))]
               ;  An empty list as json must come from an vector.
               [people  (or people (vector))])
          (route-success (sorted-map "persons" people)))))))

; Only the name and position of person are updateable.
(defendpoint "update_person" (obj)
  (cc:set-tx-metadata "txEventName" "Update Person")
  (let* ([person      (get-nested-map obj "person")]
         [id          (get-nested-string person "person_id")]
         [old-person  (storage-get-person id)]
         [new-person  (sorted-map
                          "person_id"       id
                          "name"            (get-nested-string person "name")
                          "surname"         (get-nested-string person "surname")
                          "position"        (get-nested-string person "position")
                          "roles"           (get-nested-vector old-person "roles")
                          "email"           (get-nested-string old-person "email"))])
    (cond
      ((string= "" id)
       (route-failure "Missing person id"))
      ((nil? old-person)
       (route-failure "Invalid person id"))
      (:else
        (storage-put-person new-person)
        (route-success (sorted-map "person" new-person))))))

(defun add-loan-event (loan update &optional issuer-id)
  (let* ([loan-id    (get-nested-string loan "loan_id")]
         [events     (get-nested-vector loan "event_history")]
         [event-id   (cc:make-id)]
         [now        (cc:timestamp (cc:now))]
         [event      (sorted-map "event_id" event-id
                                 "timestamp" now
                                 "issuer_person_id" issuer-id
                                 "update" update)]
         [new-events (concat 'vector events (vector event))])
    (sorted-map "loan_id" loan-id
                "event_history" new-events)))

; determine whether the partner already has a loan that is "valid".
; partners can have multiple loans assigned to them since loans may
; be cancelled, however they can have at most 1 valid loan.
; A loan is "valid" if it is not cancelled. 
; Ocassionally "active" is used as a more confusing synonym for "valid".
(defun partner-has-valid-loan? (partner)
  (let* ([invites     (select 'list
                              (lambda (event)
                                (string= (get-nested-string event "trigger_event_type")
                                         "INVITE_PARTNER"))
                              (get-nested-vector partner "event_history"))]
         [last-invite (first (reverse 'list invites))])
    (if (nil? last-invite)
       false
       (let* ([loan-id    (get-nested-string last-invite "notification" "loan_id")]
              [loan       (storage-get-loan loan-id)]
              [done-event (get-loan-event-with-type loan "mark_complete_request")])
         (if (nil? done-event)
           true
           (not
             (get-nested-bool done-event
                              "update"
                              "mark_complete_request"
                              "complete"
                              "cancelled")))))))

(defschema "create_loan"
  ; TODO:  Add loan_currency enumeration values to the schema
  ; TODO:  Ensure that executive_signoff_person_ids contains unique strings.
  ; But we need a way for tests to create more executives as needed (i.e.
  ; create_person endpoint).
  ; TODO:  Add required fields
  [""" 
  {
    "type": "object",
    "description": "Create a new loan.",
    "properties": {
      "invite_partner_request": {
        "type": "object",
        "description": "Create a new partner invitation.",
        "properties": {
          "issuer_person_email": {
            "type": "string",
            "description": "Email address of the issuer"
          },
          "partner_invitation": {
            "type": "object",
            "description": "Invitation for a partner to create a new loan.",
            "properties": {
              "facility_id": {
                "type": "string",
                "description": "ID of the loan facility."
              },
              "facility_name": {
                "type": "string",
                "description": "Name of the facility."
              },
              "loan_amount": {
                "type": "string",
                "description": "Loan amount.",
                "format": "int64"
              },
              "loan_currency": {
                "type": "string",
                "description": "Currency of the loan (inherited from facility)."
              },
              "partner_name": {
                "type": "string",
                "description": "Name of the invited partner."
              },
              "partner_surname": {
                "type": "string",
                "description": "Surname of the invited partner."
              },
              "partner_email": {
                "type": "string",
                "description": "Email of the invited partner."
              },
              "proposed_utilization_date": {
                "type": "string",
                "description": "Proposed utilization date (RFC3339)."
              },
              "executive_signoff_person_ids": {
                "type": "array",
                "description": "IDs of executives with signoff permissions for this loan.",
                "minItems": 1,
                "maxItems": 4,
                "uniqueItems": false,
                "items": {
                  "type": "string"
                }
              }
            }
          }
        }
      }
    }
  }
  """])
(defendpoint "create_loan" (obj)
  (cc:set-tx-metadata "txEventName" "New Partner Loan")
  (let* ([facilities   (storage-get-facilities)]
         [issuer-id    (get-person-id-from-email (get-nested-string obj "issuer_person_email"))]
         [invite       (get-nested-map obj "invite_partner_request" "partner_invitation")]
         [facility-id  (get-nested-string invite "facility_id")]
         [facility     (get-nested-map facilities facility-id)]
         [facility-name
           (get-default invite "facility_name"
                        (get-nested-string facility "name"))]
         [loan-amount  (to-int (get-nested-number invite "loan_amount"))]
         [currency
           (get-default invite "loan_currency"
                        (get-nested-string facility "currency"))]
         ; Update the facility's available amount
         [facility-available  (get facility "available_amount")]
         [facility            (assoc! facility "available_amount"
                                      (- (to-int facility-available) (to-int loan-amount)))]
         [facility-available  (get facility "available_amount")]
         [partner-name        (get-nested-string invite "partner_name")]
         [partner-surname     (get-nested-string invite "partner_surname")]
         [partner-email       (get-nested-string invite "partner_email")]
         [partner             (implicit-partner-from-email partner-email
                                                           :partner-name    partner-name
                                                           :partner-surname partner-surname)]
         [partner-id          (and partner (get-nested-string partner "person_id"))]
         [proposed-timestamp  (get-nested-string invite
                                                  "proposed_utilization_date")]
         [proposed-time   (cc:parse-timestamp proposed-timestamp)]
         [executive-ids   (get-nested-vector invite "executive_signoff_person_ids")]
         [non-executives  (select 'list identity
                                  (map 'list
                                       (lambda (id) (if (and (string? id) (executive-id? id)) '() id))
                                       executive-ids))])
    (assoc! invite "loan_currency" currency)
    (assoc! invite "facility_name" facility-name)
    (cond
      ((string= "" facility-id)
       (route-failure "Invalid facility_id"))
      ((not (key? facilities facility-id))
       (route-failure "Unknown facility_id"))
      ((not (string= facility-name (get-nested-string facility "name")))
       (route-failure
         (string-format "Invalid facility_name for facility_id {}"
                        facility-id)))
      ((<= loan-amount 0)
       (route-failure "Invalid loan_amount"))
      ((not (valid-currency? currency))
       (route-failure "Invalid currency"))
      ((not (string= currency (get-nested-string facility "currency")))
       (route-failure "Invalid loan_currency does not match facility"))
      ; TODO:  Add more strict validation of partner_name, partner_surname, and partner_email.
      ((string= "" partner-name)
       (route-failure "Invalid partner_name"))
      ((string= "" partner-surname)
       (route-failure "Invalid partner_surname"))
      ((string= "" partner-email)
       (route-failure "Invalid partner_email"))
      ((nil? partner-id)
       ; This may be encountered if the create_loan request specifies the email
       ; of a person that is not PARTNER.  They are not implicitly made into a
       ; partner.
       (route-failure "Invalid partner_email"))
      ((nil? proposed-time)
       (route-failure "Invalid proposed_utilization_date"))
      ((< 0 (length non-executives))
       (route-failure "Invalid id in executive_signoff_person_ids"))
      ((partner-has-valid-loan? partner)
       (route-failure "Partner already has an active loan"))
      (:else
        (let* ([facilities    (assoc! facilities facility-id facility)]
               [new-loan-id   (cc:make-id)]
               [new-loan      (sorted-map "loan_id" new-loan-id)]
               [partner-req   (sorted-map "invite_partner_request" (sorted-map "partner_invitation" invite))]
               [partner-rep   (sorted-map "invite_partner_response" (sorted-map))]
               [new-loan      (add-loan-event new-loan partner-req issuer-id)]
               [req-event-id  (get-last-event-id new-loan)]
               [new-loan      (add-loan-event new-loan partner-rep issuer-id)]
               [partner-notify  (sorted-map "person_id"             partner-id
                                            "loan_id"               new-loan-id
                                            "notification_message"  "There is a loan application to complete")]
               [partner-event  (sorted-map "event_id"           (cc:make-id)
                                           "timestamp"          (cc:timestamp (cc:now))
                                           "issuer_person_id"   issuer-id
                                           "trigger_event_id"   req-event-id
                                           "trigger_event_type" "INVITE_PARTNER"
                                           "notification"       partner-notify)])
          (storage-put-person-event partner-id partner-event)
          (storage-put-facilities facilities)
          (storage-put-loan new-loan)
          (route-success
            (sorted-map "loan" new-loan)))))))

(defendpoint "get_loans" (obj)
  (cc:set-tx-metadata "txEventName" "Get Loans")
  (let* ([ids-only?  (get-nested-bool obj "loan_ids_only")]
         [ids        (get-nested-vector obj "loan_ids")]
         [loans      (find-loans ids)])
         ;TODO: summary
         ;TODO: before
    (cond
      (ids-only? 
        (route-success
          (sorted-map "loans"
                      (or (map 'list (lambda (loan)
                                       (sorted-map "loan_id" (get-nested-string loan "loan_id")))
                               loans) (vector)))))
      (:else
        (route-success (sorted-map "loans" (or loans (vector))))))))


; Define handler functions for each update type.
; We handle create_loan separately, since in that endpoint the loan does not
; yet exist.
; TODO: move some of create_loan into the update flow.
(set 'update-handler (sorted-map))

; get-last-event-id works on both persons and loans
(defun get-last-event-id (loan)
  (get-nested-string (first (reverse 'list (get-nested-vector loan "event_history")))
                     "event_id"))

(defun get-loan-event-with-type (loan update-type)
  (first (select 'list
                 (lambda (event) (key? (get-nested-map event "update") update-type))
                 (get-nested-vector loan "event_history"))))

(defun handle-add-partner-details (loan update issuer-id)
  (let* ([loan-id        (get-nested-string loan "loan_id")]
         [loan           (add-loan-event loan update issuer-id)]
         [req-event-id   (get-last-event-id loan)]
         [resp           (sorted-map "add_partner_details_response" (sorted-map))]
         [loan           (add-loan-event loan resp issuer-id)]
         [executive-ids  (get-nested-vector (get-loan-event-with-type loan "invite_partner_request")
                                            "update"
                                            "invite_partner_request"
                                            "partner_invitation"
                                            "executive_signoff_person_ids")])
    ; TODO:  Validate the partner details (DPL2-98)

    (map ()
         ; Write person events for signoff executives, linking them to this
         ; loan.
         (lambda (executive-id)
           (let* ([exec-notify  (sorted-map "person_id"             executive-id
                                            "loan_id"               loan-id
                                            "notification_message"  "There is a loan application to approve")]
                  [exec-event  (sorted-map "event_id"           (cc:make-id)
                                           "timestamp"          (cc:timestamp (cc:now))
                                           "issuer_person_id"   issuer-id
                                           "trigger_event_id"   req-event-id
                                           "trigger_event_type" "ADD_EXECUTIVE"
                                           "notification"       exec-notify)])
             (storage-put-person-event executive-id exec-event)))
         executive-ids)

    ; TODO:  Once email notifications are implemented we need to send an
    ; email notification to the primary approver that there is a new
    ; partner loan to approve.  Secondary approvers aren't notified until
    ; the primary times out.

    loan))

(defun handle-mark-complete (loan update issuer-id)
  (let* ([loan        (add-loan-event loan update issuer-id)]
         [resp        (sorted-map "mark_complete_response" (sorted-map))]
         [cancelled?  (get-nested-bool update "mark_complete_request" "complete" "cancelled")]
         [loan        (add-loan-event loan resp issuer-id)])
    (if cancelled?
      ; Add the loan amount back to the faciltiy, that was previously subtracted 
      ; in `create_loan`
      (let* ([facilities  (storage-get-facilities)]
             [firstEvent  (get (first (get-nested-vector loan "event_history")) "update")]
             [invite      (get-nested-map firstEvent "invite_partner_request" "partner_invitation")]
             [facility-id (get-nested-string invite "facility_id")]
             [loan-amount (to-int (get-nested-number invite "loan_amount"))]
             [facility
              (get-nested-map facilities facility-id)]
             [facility-available  (get facility "available_amount")]
             [facility    (assoc! facility "available_amount"
                                  (+ (to-int facility-available) (to-int loan-amount)))]
             [facilities  (assoc! facilities facility-id facility)])
             (storage-put-facilities facilities)
             loan)
      loan)))

(assoc! update-handler "mark_complete_request" handle-mark-complete)
(assoc! update-handler "add_partner_details_request" handle-add-partner-details)

(defendpoint "update_loan" (obj)
  (cc:set-tx-metadata "txEventName" "Update Partner Loan")
  (let* ([id        (get-nested-string obj "loan_id")]
         [issuer-id (get-person-id-from-email (get-nested-string obj "issuer_person_email"))]
         [update    (get-nested-map obj "update")]
         [loans     (find-loans (vector id))]
         [loan      (first loans)])
    (cond
      ((string= "" id)
       (route-failure "Missing loan id"))
      ((nil? update)
       (route-failure "Missing update"))
      ((or (nil? loan) (= 0 (length (get-nested-vector loan "event_history"))))
       (route-failure "Missing loan"))
      (:else
        (let* ([update-name   (first (keys update))]
               [handler       (get update-handler update-name)]
               [loan          (handler loan update issuer-id)])
          (storage-update-loan loan)
          (route-success
            (sorted-map "loan" loan)))))))
