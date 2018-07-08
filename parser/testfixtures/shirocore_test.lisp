(use-package 'testing)

(test-let* "json-rpc"
     ([result          (router:make-json-rpc-response (vector "ok") ())]
      [result-with-id  (router:make-json-rpc-response (vector "ok") "test-id")]
      [e               (router:make-rpc-error router:rpc-error-req)]
      [err             (router:make-json-rpc-error e ())]
      [err-with-id     (router:make-json-rpc-error e "test-id")])
     (assert (string= (get router:json-rpc-error-message router:rpc-error-req)
                      "Invalid Request"))
     (assert (string= (get result "jsonrpc")
                      "2.0"))
     (assert (string= (get err "jsonrpc")
                      "2.0"))
     (assert (nil? (get result "id")))
     (assert (nil? (get err "id")))
     (assert (string= (get result-with-id "id")
                      "test-id"))
     (assert (string= (get err-with-id "id")
                      "test-id"))
     (assert-string= "[\"ok\"]" (json:dump-string (get result "result")))
     (assert (string= (get (get err "error") "message")
                      "Invalid Request")))

(test-let "route-success"
    ([test-success (router:route-success "ok")]
     [test-failure (router:route-failure "boo")])
    (assert (router:route-success? test-success))
    (assert (not (router:route-success? test-failure)))
    (assert (router:route-failure? test-failure))
    (assert (not (router:route-failure? test-success))))

(test "batch storage keys"
    (assert (string= (batch:storage-key-batch-prepared "mybatch") "batch:mybatch"))
    (assert (string= (batch:storage-key-batch-pending "mybatch" "batchid") "batch:mybatch~batchid"))
    (assert (string= (batch:batch-item-key "tstamp" "itemid" "") "tstamp~itemid~"))
    (assert (string= (batch:storage-key-request "reqid") "batch_request:reqid")))

(test "batch test (legacy)"
    (set 'now-string (cc:timestamp (cc:now)))
    (batch:init "test")
    (assert (nil? (second (batch:num-prepared "test" now-string))))
    (assert (= 0 (first (batch:num-prepared "test" now-string))))
    (assert (nil? (batch:prepare "test" "i123" now-string)))
    (assert (nil? (batch:prepare "test" "i456" now-string)))
    (assert (= 2 (first (batch:num-prepared "test" now-string))))
    (assert-equal (vector "i123" "i456") (batch:make-batch "test" "b789" now-string))
    (assert (= 0 (first (batch:num-prepared "test" now-string))))
    (assert (nil? (batch:remove "test" "b789" "i123")))
    (assert (nil? (batch:remove "test" "b789" "i456")))
    ; test scheduling future events
    (set 'later-string (cc:timestamp (cc:add-seconds (cc:now) 1)))
    (assert (nil? (batch:prepare "test" "i012" later-string)))
    (assert (= 0 (first (batch:num-prepared "test" now-string))))
    ; try empty batch
    (assert-equal (vector) (batch:make-batch "test" "b012" now-string))
    (assert-equal (vector "i012") (batch:make-batch "test" "b123" later-string)))

(test "batch test"
    (cc:set-app-property batch:property-paused true)
    (assert (batch:paused?))
    (cc:set-app-property batch:property-paused false)
    (assert (not (batch:paused?)) "false is not paused {}" (cc:get-app-property batch:property-paused))
    (cc:set-app-property batch:property-paused ())
    (assert (not (batch:paused?)) "nil is not paused")
    (cc:set-app-property batch:property-paused "x")
    (assert (batch:paused?)))
