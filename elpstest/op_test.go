package elpstest

import "testing"

func TestSpecialOp(t *testing.T) {
	tests := TestSuite{
		{"if", TestSequence{
			// if operator
			{"(if () 1 2)", "2"},
			{"(if t 1 2)", "1"},
			{"(if t (set 'x 1) (set 'x 2))", "1"},
			{"x", "1"},
			{"(if () (set 'x 1) (set 'x 2))", "2"},
			{"x", "2"},
			{"(if '(()) 1 2)", "1"},
			{`(if "false" 1 2)`, "1"},
		}},
		{"cond", TestSequence{
			{`(cond)`, "()"},
			{`(cond (else 1))`, "1"},
			{`(cond (t 2) (else 1))`, "2"},
			{`(cond ((< 1 2) 3) (else 1))`, "3"},
		}},
		{"expr", TestSequence{
			{`((expr ()))`, "()"},
			{`((expr "hello"))`, `"hello"`},
			{`((expr %) 123)`, "123"},
			{`((expr %2) 'a 'b)`, "'b"},
			{`((expr (reverse %&)) 1 2 3)`, "'(3 2 1)"},
		}},
	}
	RunTestSuite(t, tests)
}
