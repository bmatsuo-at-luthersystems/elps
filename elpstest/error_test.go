package elpstest

import "testing"

func TestErrors(t *testing.T) {
	tests := TestSuite{
		{"ignore-errors", TestSequence{
			// silencing errors and returning nil.
			{`(ignore-errors (progn 0 (error 'test-error "test message") 1))`, `()`, ""},
		}},
		{"handler-bind", TestSequence{
			// handling specific types of errors and returning meaningful data.
			{`(handler-bind ((condition list))
				(progn
					(debug-print "do stuff")
					(error 'custom-error "custom data")))`,
				`'('custom-error "custom data")`, "\"do stuff\"\n"},
			{`(handler-bind ((custom-error (lambda (c &rest _) 1)) (condition list))
				(progn
					(debug-print "do stuff")
					(error 'custom-error "custom data")))`,
				`1`, "\"do stuff\"\n"},
			{`(handler-bind ((custom-error (lambda (c &rest _) 1)) (condition list))
				(progn
					(debug-print "do stuff")
					(error 'other-error "other data")))`,
				`'('other-error "other data")`, "\"do stuff\"\n"},
		}},
	}
	RunTestSuite(t, tests)
}
