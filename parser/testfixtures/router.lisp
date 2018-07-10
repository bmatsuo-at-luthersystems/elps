(in-package 'router)

; The routes map stores endpoint handlers.
(set 'routes (sorted-map))

; The router-args map is used to verify the number of arguments in a handler.
(set 'router-args (sorted-map))

; The router-schemas maps is used to validate argument values in a handler.
(set 'router-schemas (sorted-map))

; phylum-key is the key under which the phylum code is persisted.
(set 'phylum-key "luther:phylum")

(defun default-phylum () (base64:encode """
(in-package 'user)
(use-package 'router)

(defendpoint init ()
  (route-success ()))

(defendpoint healthcheck ()
  (cc:warnf () "The healthcheck endpoint has not been configured")
  (route-failure ()))
"""))

(set 'request-schema """
{
    "$schema": "http://json-schema.org/draft-04/schema#",
    "description": "A JSON RPC 2.0 request",
    "oneOf": [
        {
            "description": "An individual request",
            "$ref": "#/definitions/request"
        }
    ],
    "definitions": {
        "request": {
            "type": "object",
            "required": [ "jsonrpc", "method" ],
            "properties": {
                "jsonrpc": { "enum": [ "2.0" ] },
                "method": {
                    "type": "string"
                },
                "id": {
                    "type": [ "string", "number", "null" ]
                },
                "params": {
                    "type": [ "array", "object" ]
                }
            }
        }
    }
}
""")

; rpc-error-* is an rpc error code and has corresponding entries in
; rpc-error-code and json-rpc-error-message maps.
(set 'rpc-error-parse    'rpc-error-parse)
(set 'rpc-error-req      'rpc-error-req)
(set 'rpc-error-method   'rpc-error-method)
(set 'rpc-error-params   'rpc-error-params)
(set 'rpc-error-internal 'rpc-error-internal)
(set 'rpc-error-server   'rpc-error-server)

(set 'rpc-error-code
     (sorted-map
       rpc-error-parse     -32700
       rpc-error-req       -32600
       rpc-error-method    -32601
       rpc-error-params    -32602
       rpc-error-internal  -32603
       rpc-error-server    -32000))

(set 'json-rpc-error-message
     (sorted-map
       rpc-error-parse     "Parse error"
       rpc-error-req       "Invalid Request"
       rpc-error-method    "Method not found"
       rpc-error-params    "Invalid params"
       rpc-error-internal  "Internal error"
       rpc-error-server    "Server error"))

(defun make-rpc-error (code)
  (sorted-map
    "code"     (get rpc-error-code code)
    "message"  (get json-rpc-error-message code)))

(defun make-json-rpc-error (err id)
  (let ([resp (sorted-map "jsonrpc"  "2.0"
                          "error"    err
                          "id"       id)])
    resp))

(defun make-json-rpc-response (result id)
  (let ([resp (sorted-map "jsonrpc"  "2.0"
                          ; Serialize the result data as a raw json message
                          ; early so that when we can explicitly specify
                          ; :string-numbers false to ensure that the json-rpc
                          ; "id" field has the correct value.
                          "result"   (json:dump-message result)
                          "id"       id)])
    resp))

(defun set-args (endpoint args)
  (assoc! router-args endpoint args))

(defun set-handler (endpoint handleFn)
  (assoc! routes endpoint handleFn))

; schemas-match? returns true if args pass schemas.
(defun schemas-match? (schemas args)
  (let* ([tuples (zip 'list schemas args)]
         [valid? (curry-function apply 'cc:valid-json?)])
    (all? valid? tuples)))

; handle-request gets the parsed json-rpc envelope and the raw request json so
; that it can be parsed using the environment setting of
; json:use-string-numbers.
(defun handle-request (req-json envelope)
    (cond
        ((not (sorted-map? envelope))
            (make-json-rpc-error (make-rpc-error rpc-error-req) ()))
        ((cc:valid-json? request-schema req-json)
            (let*
                ([method-name  (get envelope "method")]
                 [id           (get envelope "id")]
                 ; phylum-req may get parsed with :string-numbers implied true
                 ; if the phylum wants it that way.
                 [phylum-req   (json:load-bytes req-json)]
                 [params       (get phylum-req "params")]
                 [rep-pair     (run-phylum method-name params)]
                 [rep          (first rep-pair)]
                 [err          (second rep-pair)])
                (cond
                    ((nil? id)   ())
                    ((nil? err)
                      (make-json-rpc-response rep id))
                    (:else
                      (make-json-rpc-error err id)))))
        (:else (let ([id (get envelope "id")])
                (make-json-rpc-error (make-rpc-error rpc-error-req) id)))))

; run-phylum loads the shiro from underlying storage and executes it by calling
; the handler.
(defun run-phylum (method-name args)
    (let* ([method-name (or method-name "")])
        ; This is scope of shiro
        (let*
            ([method-args  (get router-args method-name)]
             [pos-args     (norm-args method-args args)]
             [handle       (get routes method-name)])
            (cond
                ; run default handler and ignore args
                ((nil? handle)     ((get routes "default")))
                ((= (length pos-args) (length method-args))
                    (apply handle pos-args))
                (:else  (route-error-code rpc-error-params))))))

; norm-args converts positional args and named args to positional args.
(defun norm-args (method-args args)
    (let* ([str-method-args (map 'list to-string method-args)])
        (cond
            ; already positional args
            ((list? args)  args)
            ((array? args)  (map 'list identity args))
            ; TODO: implement list membership function
            ((all? (lambda (arg) (any? (expr (string= % arg)) str-method-args)) (keys args))
                (map 'list
                     (lambda (arg)
                       (get args arg))
                     str-method-args))
            ; missing args
            (:else ()))))

(defun route-success (result)
    (list result ()))

(defun route-error-code (code)
    (list () (make-rpc-error code)))

(defun store-phylum (phylum-encoded)
  (cc:storage-put phylum-key phylum-encoded))

(defun load-phylum ()
  (let* ([phylum-encoded  (cc:storage-get phylum-key)]
         [phylum-bytes    (base64:decode phylum-encoded)])
    (load-bytes phylum-bytes)
    (in-package 'router)))

(set 'init-pre-hooks '())

; callback-list takes a list of callbacks and calls each in order.
(set 'callback-list (map ()
                         (lambda (callback) (callback))))

(defun exec-init-pre-hooks ()
    (callback-list init-pre-hooks))

(defun valid-args? (endpoint args)
  (let* ([schemas    (get router-schemas endpoint)]
         [json-args  (map 'list json:dump-bytes args)])
    (or (nil? schemas)
        (schemas-match? schemas json-args))))

(defun json-response-bytes (response)
  ; The JSON-RPC spec demands that :string-numbers be false here.  Chaincode
  ; may want to specify (json:use-string-numbers true) if they must deal with
  ; large/precise numeric quantities in JSON.  So we have to override that
  ; here, and take precaution elsewhere that chaincode functions don't get
  ; receive/output numeric values that should actually be strings.
  (json:dump-bytes response :string-numbers false))

(export 'defendpoint)
(defmacro defendpoint (name args &rest exprs)
  ; because defendpoint receives unquoted symbols as arguments it has to
  ; quote the argumunts to register-endpoint after unquoting (which resolves
  ; the macro's internal binding of name={user-provided-symbol}.
  (quasiquote
    (router:register-endpoint (quote (unquote name))
                              (map 'list to-string (quote (unquote args)))
                              (lambda (unquote args)
                                (unquote-splicing exprs)))))

(defun register-endpoint (endpoint arg-names handle-fun)
  (cc:debugf (sorted-map "endpoint" endpoint) "Registered endpoint")
  (let* ([endpoint-str (to-string endpoint)]
         [handler (lambda (&rest args)
                    (cc:set-tx-metadata "txEventName" endpoint-str)
                    (cc:set-tx-metadata "txTimestamp" (cc:timestamp (cc:now)))
                    (if (valid-args? endpoint-str args)
                      (progn
                        (cc:infof (sorted-map "endpoint" endpoint) "Handler found")
                        (apply handle-fun args))
                      (route-error-code rpc-error-params)))])
         (set-args endpoint-str arg-names)
         (set-handler endpoint-str handler)))

(export 'defschema)
(defmacro defschema (name &rest schemas)
  (quasiquote
    (router:register-schema (quote (unquote name))
                            (unquote-splicing schemas))))

(defun register-schema (endpoint &rest schemas)
  ; the list of schemas gets flattened (at one level of depth) to ensure
  ; backwards compatability and to keep things from getting out of hand...
  (let* ([ls (lambda (schema)
              (cond
                ((list? schema)    schema)
                ((vector? schema)  schema)
                (else              (list schema))))]
         [schemas (apply concat 'list (map 'list ls schemas))])
      (if (all? 'cc:valid-schema? schemas)
        (assoc! router-schemas (to-string endpoint) schemas)
        (error 'cc:invalid-schema
               (format-string "invalid json-schema for endpoint: {}" endpoint)))))

(export 'route-success)
(defun route-success (resp)
  (list resp ()))

(export 'route-failure)
(defun route-failure (data)
  (let ([err (make-rpc-error rpc-error-server)])
    ; Serialize the error data as a raw json message early so that when we can
    ; explicitly specify :string-numbers false to ensure that the json-rpc "id"
    ; field has the correct value.
    (if (not (nil? data))
      (assoc! err "data" (json:dump-message data))
      ())
    (list () err)))

(export 'route-error-code)
(defun route-error-code (code)
  (list () (make-rpc-error code)))

(export 'add-init-pre-hook)
(defun add-init-pre-hook (callback)
  (set 'init-pre-hooks (concat 'list init-pre-hooks (list callback))))

(defun route-success? (resp)
  (nil? (second resp)))

(defun route-failure? (resp)
  (not (route-success? resp)))

; cc-error is called if an execution step returned an error.  This is called by
; Go.
(defun cc-error (req-json rpc-err)
  ; we treat server errors as fatal, spoils the entire batch
  (let* ([req (ignore-errors (json:load-bytes req-json))]
         [rpc-err (if (sorted-map? rpc-err) rpc-err (make-rpc-error rpc-error-server))])
        (if (not (sorted-map? req))
          (json-response-bytes (make-json-rpc-error rpc-err ()))
          (json-response-bytes (make-json-rpc-error rpc-err (get req "id"))))))

; cc-init is called when the chaincode is deployed or upgraded.  This is called
; by Go.
(defun cc-init (&optional req-json)
  (let*
    ([req    (ignore-errors (json:load-bytes req-json))]
     [params (if (sorted-map? req) (get req "params") ())]
     [code   (if params
               (first params)
               (default-phylum))])
    (store-phylum code)
    (load-phylum)
    (exec-init-pre-hooks)
    (let
      ([resp (run-phylum "init" [])])
      (if (route-success? resp)
        ()
        (cc-error req-json (second resp))))))

(defun cc-handle (&optional req-json)
    (load-phylum)
    ; Initially parse req-json with :string-numbers false so that the json-rpc
    ; "id" field is properly decoded for handle-request.
    (let ([reqs  (handler-bind ([json:syntax-error (lambda (&rest xs) ())])
                   (and req-json
                        (json:load-bytes req-json :string-numbers false)))])
        (cond
            ((nil? reqs)
                (json-response-bytes (make-json-rpc-error
                                       (make-rpc-error rpc-error-parse)
                                       ())))
            ((and (vector? reqs) (= (length reqs) 0))
                (json-response-bytes (make-json-rpc-error
                                       (make-rpc-error rpc-error-req)
                                       ())))
            ((vector? reqs)
                (let* ([handler (lambda (req)
                                  (let ([req-json (json:dump-bytes req :string-numbers false)])
                                    (handle-request req-json req)))]
                       [rep  (select 'vector identity (map 'list handler reqs))])
                  (if (= (length rep) 0)
                    ()
                    (json-response-bytes rep))))
            (:else
                (let
                  ([rep (handle-request req-json reqs)])
                  (if (nil? rep)
                    ()
                    (json-response-bytes rep)))))))

(defendpoint default ()
  (route-error-code rpc-error-method))

(defendpoint update (code)
  (cc:set-tx-metadata "txUpdate" "true")
  (store-phylum code)
  (load-phylum)
  (exec-init-pre-hooks)
  (run-phylum "init" []))

(defendpoint vbc_get_blockchain_info ()
  (route-success (cc:blockchain-info)))

(defendpoint vbc_get_blocks (blocks transactions)
  (let ([blocks (cc:blocks (map 'list identity blocks)
                           (map 'list identity transactions))])
    (if (nil? blocks)
      (route-failure "missing blocks or transactions")
      (route-success blocks))))

(defendpoint get_app_control_property (name)
  (route-success (cc:get-app-property name)))

(defendpoint set_app_control_property (name value)
  (route-success (cc:set-app-property name value)))
