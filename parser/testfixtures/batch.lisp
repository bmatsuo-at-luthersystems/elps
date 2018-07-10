(in-package 'batch)
(use-package 'router)

(set 'storage-key-prefix-batch "batch:")

(set 'storage-key-prefix-request "batch_request:")

; property-paused is used to store pausing state on app control.
(set 'property-paused "BATCHING_PAUSED")

; key-separator must not appear in RFC3339 or a UUID, and must be > [a-zA-Z0-9]
(set 'key-separator "~")

; responseHandlers stores response handlers for batches.
(set 'response-handlers (sorted-map))

(defun storage-key-batch-prepared (name)
  (string:join (list storage-key-prefix-batch (to-string name)) ""))

(defun storage-key-batch-pending (batch-name batch-id)
  (string:join (list storage-key-prefix-batch (to-string batch-name) key-separator (to-string batch-id)) ""))

(defun batch-item-key (timestamp item-id x)
  (string:join (list timestamp item-id x) key-separator))

(defun storage-key-request (req-id)
  (string:join (list storage-key-prefix-request req-id) ""))

(defun storage-load-json (key)
  (let ([data (cc:storage-get key)])
    (ignore-errors (json:load-bytes data))))

(defun storage-put-json (key data)
  (cc:storage-put key (json:dump-bytes data)))

(defun route-handler-num-requests (batch-name)
  (if (paused?)
    (progn
      (cc:infof (sorted-map) "paused getting num requests for {}" batch-name)
      (route-success 0))
    (let*
      ([rep   (num-prepared batch-name (cc:timestamp (cc:now)))]
       [prep  (first rep)]
       [err   (second rep)])
      (if (nil? err)
        (route-success prep)
        (progn
          (cc:errorf (sorted-map "batch_name" batch-name
                                 "request_id" req-id)
                     err)
          (route-failure ()))))))

(defun route-handler-get-requests (batch-name)
  (if (paused?)
    (route-success (vector))
    (let*
      ([batch-id (cc:make-id)]
       [req-ids  (make-batch batch-name batch-id (cc:timestamp (cc:now)))]
       [fn       (lambda (req-id)
                   (let*
                     ([key  (string:join (list storage-key-prefix-request req-id) "")]
                      [req  (storage-load-json key)])
                     (assoc! req "batch_id" batch-id)
                     (assoc! req "request_id" req-id)))])
      (route-success (map 'vector fn req-ids)))))

(defun response-handler (batch-name rep)
  (let*
    ([handler   (get-default response-handlers batch-name response-handler-default)]
     [batch-id  (get-default rep "batch_id" "")]
     [req-id    (get-default rep "request_id" "")]
     [err       (remove batch-name batch-id req-id)])
    (if (nil? err)
      (handler rep)
      (progn
        (cc:errorf (sorted-map "batch_name" batch-name
                               "request_id" req-id)
                   err)
        (route-failure ())))))

(defun response-handler-default (batch-name)
  (cc:errorf (sorted-map "batch_name" batch-name)
             "No handler registered for batch")
  (router-failure ()))

(defun get-id (key)
  (second (string:split key key-separator)))

(defun timestamp-to-unix (stamp)
  (let
    ([t0 (cc:parse-timestamp stamp)])
    (if (nil? t0)
      ()
      (cc:timestamp-unix t0))))

(export 'paused)
(defun paused? ()
  (not (not (cc:get-app-property property-paused))))

(export 'init)
(defun init (batch-name)
  (let
    ([prep-key (storage-key-batch-prepared batch-name)])
    (storage-put-json prep-key (vector))))

(export 'handler)
(defun handler (name fun)
  (add-init-pre-hook (lambda () (batch:init (to-string name))))
  (assoc! response-handlers (to-string name) fun))

; returns the index it which s should be found in sorted list string-list
(defun sorted-string-position (string-list s)
  (search-sorted (length string-list)
                 (lambda (i) (string<= s (nth string-list i)))))

(export 'num-prepared)
(defun num-prepared (batch-name timestamp)
  (let*
    ([prep-key       (storage-key-batch-prepared batch-name)]
     [prep-ids       (storage-load-json prep-key)]
     [norm-timestamp (timestamp-to-unix timestamp)]
     [prep-item-now  (batch-item-key norm-timestamp "" "")])
    (cond
      ((nil? prep-ids)
        (list () "Missing batch"))
      ((nil? norm-timestamp)
        (list () "Invalid timestamp"))
      (:else
        (list (sorted-string-position prep-ids prep-item-now) ())))))

(export 'prepare)
(defun prepare (name item-id when-timestamp)
  (let*
    ([prep-key        (storage-key-batch-prepared name)]
     [prep-ids        (storage-load-json prep-key)]
     [norm-timestamp  (timestamp-to-unix when-timestamp)]
     [item-key        (batch-item-key norm-timestamp item-id "")])
    (cond
      ((nil? prep-ids)
        "Missing batch")
      ((nil? norm-timestamp)
        "Invalid timestamp")
      (:else
        (storage-put-json prep-key
                          (insert-sorted 'vector prep-ids string< item-key))))))

(export 'make-batch)
(defun make-batch (name batch-id timestamp)
  (let*
    ([prep-key        (storage-key-batch-prepared name)]
     [prep-ids        (storage-load-json prep-key)]
     [pend-key        (storage-key-batch-pending name batch-id)]
     [pend-ids        (storage-load-json pend-key)]
     [norm-timestamp  (timestamp-to-unix timestamp)]
     [prep-item-now   (batch-item-key norm-timestamp "" "")]
     [prep-index      (sorted-string-position prep-ids prep-item-now)]
     [due-ids         (map 'list get-id (slice 'vector prep-ids 0 prep-index))]
     [prep-ids-new    (slice 'vector prep-ids prep-index (length prep-ids))]
     [pend-ids-new    (concat 'vector pend-ids due-ids)]
     )
    (cond
      ((nil? norm-timestamp)
       ())
      ((= 0 (length pend-ids-new))
       pend-ids-new)
      (:else
        (progn
          (storage-put-json prep-key prep-ids-new)
          (storage-put-json pend-key pend-ids-new)
          pend-ids-new)))))

(defun remove-string (type-specifier lis s)
  (reject type-specifier (expr (string= s %)) lis))

(export 'remove)
(defun remove (name batch-id item)
  (let*
    ([pend-key (storage-key-batch-pending name batch-id)]
     [pend-ids (storage-load-json pend-key)])
    (if (nil? pend-ids)
      "Missing batch"
      (let*
        ([pend-ids-new (remove-string 'vector pend-ids item)])
        (cond
          ((= (length pend-ids-new) 0)
            (cc:storage-put pend-key ""))
          ((= (length pend-ids-new) (length pend-ids))
            "Missing item")
          (:else
            (storage-put-json pend-key pend-ids-new)))))))

(export 'schedule-request)
(defun schedule-request (name req when-timestamp)
  (let*
    ([req-id          (cc:make-id)]
     [req-key         (storage-key-request req-id)]
     [norm-timestamp  (timestamp-to-unix when-timestamp)])
    (if (nil? norm-timestamp)
      "Invalid timestamp"
      (progn
        (storage-put-json req-key req)
        (prepare name req-id when-timestamp)))))

(add-init-pre-hook (lambda () (cc:set-app-property property-paused false)))

(defendpoint batch_get_num_requests (batch_name)
             (route-handler-num-requests batch_name))

(defendpoint batch_get_requests (batch_name)
             (cc:set-tx-metadata "txBatchName" batch_name)
             (route-handler-get-requests batch_name))

(defendpoint batch_process_response (batch_name rep)
             (cc:set-tx-metadata "txBatchName" batch_name)
             (response-handler batch_name rep))
