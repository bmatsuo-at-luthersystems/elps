package elpstest

import "testing"

func TestMaps(t *testing.T) {
	tests := TestSuite{
		{"sorted-map", TestSequence{
			// simple lexical scoping tests
			{"(set 'm (sorted-map 'b 1 'a 2))", "(sorted-map a 2 b 1)"},
			{"(assoc! m 'a 0)", "(sorted-map a 0 b 1)"},
			{"(assoc m 'b 2)", "(sorted-map a 0 b 2)"},
			{"(get m 'b)", "1"},
		}},
	}
	RunTestSuite(t, tests)
}
