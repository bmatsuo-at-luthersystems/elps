package elpstest

import "testing"

func TestErrors(t *testing.T) {
	tests := TestSuite{
		{"ignore-errors", TestSequence{
			// silencing errors and returning nil.
			{`(ignore-errors (progn 0 (error 'test-error "test message") 1))`, `()`},
		}},
		{"handler-bind", TestSequence{
			// handling specific types of errors and returning meaningful data.
			{`(handler-bind ((condition identity))
				(progn
					(debug-print "do stuff")
					(error 'custom-error "custom data")))`,
				`"custom data"`},
			{`(handler-bind ((custom-error (lambda (&rest _) 1)) (condition identity))
				(progn
					(debug-print "do stuff")
					(error 'custom-error "custom data")))`,
				`1`},
			{`(handler-bind ((custom-error (lambda (&rest _) 1)) (condition identity))
				(progn
					(debug-print "do stuff")
					(error 'other-error "other data")))`,
				`"other data"`},
		}},
	}
	RunTestSuite(t, tests)
}
