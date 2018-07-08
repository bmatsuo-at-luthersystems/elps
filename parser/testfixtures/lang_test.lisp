(use-package 'testing)

(test "string lists"
      (assert (= 0 (batch:sorted-string-position [] "")))
      (assert (= 2 (batch:sorted-string-position ["a" "b"] "c")))
      (assert (= 1 (batch:sorted-string-position ["a" "b"] "b")))
      (assert (= 2 (batch:sorted-string-position ["a" "b" "d"] "c")))
      (assert (equal? (vector "a" "b" "c" "d") (insert-sorted 'vector ["a" "b" "d"] string< "c")))
      (assert (equal? (vector "a") (insert-sorted 'vector [] string< "a")))
      (assert (equal? (vector "a" "b" "d") (batch:remove-string 'vector ["a" "b" "c" "d"] "c")))
      (assert (equal? (vector "a" "b" "d") (batch:remove-string 'vector ["a" "b" "d"] "c")))
      (assert (equal? (vector) (batch:remove-string 'vector ["a"] "a"))))

(test "dates"
	(set 'bad-stamp (cc:parse-timestamp "2018-03-17T20:59:27-07:0011"))
	(assert (nil? bad-stamp))
	(set 'now (cc:parse-timestamp "2018-03-17T20:59:27-07:00"))
	(assert (not (nil? now)))
	(set 'later (cc:add-seconds now 1))
	(assert (string= "2018-03-18T03:59:28Z" (cc:timestamp later)))
	(set 'later (cc:add-years now 1))
	(assert (string= "2019-03-18T03:59:27Z" (cc:timestamp later)))
	(assert (string= "5C8F179F" (cc:timestamp-unix later)))
	(set' long-time-ago (cc:parse-timestamp "1971-03-17T20:59:27-07:00"))
	(assert (string= "02459D9F" (cc:timestamp-unix long-time-ago))))

(test "metadata" 
    (set 'key-tx-desc "luther:tx_desc")
    (set 'tx-desc "test-value")
	(cc:set-tx-metadata key-tx-desc tx-desc)
	(assert (string= (cc:get-tx-metadata key-tx-desc) tx-desc)))

(test "app control properties"
	(assert (not (batch:paused?)))
	(cc:set-app-property 'TEST_STRING "str")
	(assert (string= (cc:get-app-property 'TEST_STRING) "str"))
	(assert (string= (cc:get-app-property "TEST_STRING") "str"))
	(assert (nil? (cc:get-app-property "BOGUS")))
	(cc:set-app-property "TEST_BOOL" true)
	(assert (cc:get-app-property 'TEST_BOOL))
	(cc:set-app-property "TEST_BOOL" ())
	(assert (not (cc:get-app-property 'TEST_BOOL))))

(test "storage"
	(cc:storage-put "unknown" "")
	(cc:storage-put "zk_a" (json:dump-bytes "v_1"))
	(cc:storage-put "zk_b" (json:dump-bytes "v_2"))
	(cc:storage-put "zk_c" (json:dump-bytes "v_3"))
	; FIXME: There is a bug in mock stub that treats the query as
	; [inclusive, inclusive], whereas the docs specify it should be
	; [inclusive, exclusive). Once the mock stub is un-fucked we 
	; will explicitly test exclusivity where '+' is used.
	(assert (equal? (json:load-bytes (cc:storage-get "zk_a")) "v_1"))
	(defun reduce-append (acc k v)
		(concat 'list
                acc
                (list (list k (json:load-bytes v)))))
    (defun storage-collect-range (start end)
      (cc:storage-reduce-range reduce-append () start end))
	; [zk_a:zk_b] need to use zk prefix to avoid random keys in namespace
	(assert (equal? 
		[["zk_a" "v_1"] ["zk_b" "v_2"]] 
		(storage-collect-range "zk_a" "zk_b+")))
	; [zk_d:*]
	(assert (equal?
		()
		(storage-collect-range "zk_d" "")))
	; [zk_b:*]
	(assert (equal? 
		[["zk_b" "v_2"] ["zk_c" "v_3"]] 
		(storage-collect-range "zk_b" "")))
	(cc:storage-put "aa_a" (json:dump-bytes "v_1"))
	(cc:storage-put "aa_b" (json:dump-bytes "v_2"))
	; [*:aa_b] need to use aa_ prefix to avoid random keys in namespace
	(assert-equal  [["aa_a" "v_1"] ["aa_b" "v_2"]]
                 (storage-collect-range "" "aa_b+"))
	(cc:storage-put "aa_b" "")
	; [*:aa_b] need to use aa_ prefix to avoid random keys in namespace
	(assert (equal? 
		[["aa_a" "v_1"]] 
		(storage-collect-range "" "aa_b"))))
