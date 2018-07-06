package elpstest

import "testing"

func TestSort(t *testing.T) {
	tests := TestSuite{
		{"sort lists", TestSequence{
			// sorting primitive values
			{"(set 'lis '(3 1 2 5 4))", "'(3 1 2 5 4)", ""},
			{"(stable-sort < lis)", "'(1 2 3 4 5)", ""},
			{"(stable-sort < lis identity)", "'(1 2 3 4 5)", ""},
			{"(stable-sort > lis)", "'(5 4 3 2 1)", ""},
			{"(stable-sort < lis -)", "'(5 4 3 2 1)", ""},
			{"(set 'sort-asc (stable-sort <))", "<builtin>", ""},
			{"(set 'sort-desc (stable-sort >))", "<builtin>", ""},
			{"(sort-asc lis)", "'(1 2 3 4 5)", ""},
			{"(sort-desc lis)", "'(5 4 3 2 1)", ""},
		}},
		{"sort complex lists", TestSequence{
			// sorting structured values
			{"(set 'lis '('(3 'c) '(1 'a) '(2 'b)))", "'('(3 'c) '(1 'a) '(2 'b))", ""},
			{"(stable-sort < lis first)", "'('(1 'a) '(2 'b) '(3 'c))", ""},
		}},
		{"insert-sorted", TestSequence{
			// inserting into sorted lists
			{"(set 'lis '(1 2 3 4 5))", "'(1 2 3 4 5)", ""},
			{"(insert-sorted 'list lis < 2.5)", "'(1 2 2.5 3 4 5)", ""},
			{"(insert-sorted 'list lis < 2.5 identity)", "'(1 2 2.5 3 4 5)", ""},
			{"lis", "'(1 2 3 4 5)", ""},
			{"(set 'lis '('(1 'a) '(2 'b) '(3 'c)))", "'('(1 'a) '(2 'b) '(3 'c))", ""},
			{"(insert-sorted 'list lis < '(2.5 'ba) first)", "'('(1 'a) '(2 'b) '(2.5 'ba) '(3 'c))", ""},
		}},
		{"insert-sorted", TestSequence{
			// inserting into sorted vectors
			{"(set 'vec (vector 1 2 3 4 5))", "(vector 1 2 3 4 5)", ""},
			{"(insert-sorted 'vector vec < 2.5)", "(vector 1 2 2.5 3 4 5)", ""},
			{"(insert-sorted 'vector vec < 2.5 identity)", "(vector 1 2 2.5 3 4 5)", ""},
			{"vec", "(vector 1 2 3 4 5)", ""},
			{"(set 'vec (vector '(1 'a) '(2 'b) '(3 'c)))", "(vector '(1 'a) '(2 'b) '(3 'c))", ""},
			{"(insert-sorted 'vector vec < '(2.5 'ba) first)", "(vector '(1 'a) '(2 'b) '(2.5 'ba) '(3 'c))", ""},
		}},
	}
	RunTestSuite(t, tests)
}
