package elpstest

import "testing"

func TestSort(t *testing.T) {
	tests := TestSuite{
		{"sort lists", TestSequence{
			// simple lexical scoping tests
			{"(set 'lis '(3 1 2 5 4))", "'(3 1 2 5 4)"},
			{"(sort < lis)", "'(1 2 3 4 5)"},
			{"(sort > lis)", "'(5 4 3 2 1)"},
			{"(set 'sort-asc (sort <))", "<builtin>"},
			{"(set 'sort-desc (sort >))", "<builtin>"},
			{"(sort-asc lis)", "'(1 2 3 4 5)"},
			{"(sort-desc lis)", "'(5 4 3 2 1)"},
		}},
	}
	RunTestSuite(t, tests)
}
