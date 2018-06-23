package elpstest

import "testing"

func TestArray(t *testing.T) {
	tests := TestSuite{
		{"vector", TestSequence{
			{"(vector)", "(vector)", ""},
			{"(vector 1 2 3)", "(vector 1 2 3)", ""},
			{"(vector (vector 1 2 3))", "(vector (vector 1 2 3))", ""},
			{`(aref (vector 'a 'b 'c) 0)`, "'a", ""},
			{`(aref (vector 1 2 3) 2)`, "3", ""},
			{`(ignore-errors (aref (vector 1 2 3) 3))`, "()", ""},
			{`(ignore-errors (aref (vector 1 2 3) -1))`, "()", ""},
			{"(ignore-errors (nth (vector) -1))", "()", ""},
			{"(nth (vector) 0)", "()", ""},
			{"(nth (vector) 1)", "()", ""},
			{"(nth (vector) 2)", "()", ""},
			{"(nth (vector 1) 0)", "1", ""},
			{"(nth (vector 1) 1)", "()", ""},
			{"(nth (vector 1) 2)", "()", ""},
		}},
	}
	RunTestSuite(t, tests)
}
