package elpstest

import "testing"

func TestLoad(t *testing.T) {
	tests := TestSuite{
		{"load simple strings", TestSequence{
			{`(load-string "(+ 2 3)")`, "5"},
			{`((load-string "(lambda (x) (* x 2))") 4)`, "8"},
			{`(load-string "(defun double (x) (* x 2))")`, "()"},
			{`(double 3)`, "6"},
			{`(load-string "(defun double (x) (- (* x 2)))")`, "()"},
			{`(double 3)`, "-6"},
		}},
	}
	RunTestSuite(t, tests)
}
