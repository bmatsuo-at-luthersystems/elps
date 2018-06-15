package elpstest

import "testing"

func TestArray(t *testing.T) {
	tests := TestSuite{
		{"vector", TestSequence{
			{"(vector 1 2 3)", "(vector 1 2 3)"},
			{`(aref (vector 'a 'b 'c) 0)`, "'a"},
			{`(aref (vector 1 2 3) 2)`, "3"},
		}},
	}
	RunTestSuite(t, tests)
}
