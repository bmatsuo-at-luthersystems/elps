package elpstest

import "testing"

func TestMath(t *testing.T) {
	tests := TestSuite{
		{"arithmetic", TestSequence{
			// arithmetic functions w/o args
			{"(+)", "0"},
			{"(-)", "0"},
			{"(*)", "1"},
			{"(/)", "1"},
			// arithmetic functions w/ one arg
			{"(+ 2)", "2"},
			{"(+ 2.0)", "2"},
			{"(- 2)", "-2"},
			{"(- 2.0)", "-2"},
			{"(* 2)", "2"},
			{"(* 2.0)", "2"},
			{"(/ 2)", "0.5"},
			{"(/ 2.0)", "0.5"},
			// arithmetic functions w/ two args
			{"(* 2 0.75)", "1.5"},
			{"(+ 1 2 3)", "6"},
			{"(+ 1 (* 2 3))", "7"},
			{"(+ 1 1.5)", "2.5"},
			{"(- 0.5 1)", "-0.5"},
			{"(* 2 0.75)", "1.5"},
			{"(pow 2 2)", "4"},
			{"(pow 2 3)", "8"},
			{"(pow 2.0 3.0)", "8"},
			{"(mod 2 2)", "0"},
			{"(mod 2 3)", "2"},
			{"(mod 4 3)", "1"},
			{"(mod -1 3)", "-1"},
			{"(mod -5 3)", "-2"},
		}},
	}
	RunTestSuite(t, tests)
}

func TestLogic(t *testing.T) {
	tests := TestSuite{
		{"logic", TestSequence{
			{"(not true)", "false"},
			{"(not ())", "true"},
			{"(not false)", "true"},
			{"(= 1 1.0)", "true"},
			{"(= 1 1)", "true"},
			{"(= 5 1)", "false"},
			{"(= 5.0 (+ 1 4.0))", "true"},
			{"(< 0 1)", "true"},
			{"(< 0 1.0)", "true"},
			{"(< 1 1)", "false"},
			{"(< 1.0 1)", "false"},
			{"(< 2 1)", "false"},
			{"(< 2.0 1)", "false"},
			{"(<= 0 1)", "true"},
			{"(<= 0 1.0)", "true"},
			{"(<= 1 1)", "true"},
			{"(<= 1.0 1)", "true"},
			{"(<= 2 1)", "false"},
			{"(<= 2.0 1)", "false"},
			{"(> 0.0 1)", "false"},
			{"(> 0 1.0)", "false"},
			{"(> 1 1)", "false"},
			{"(> 1.0 1)", "false"},
			{"(> 2 1)", "true"},
			{"(> 2.0 1)", "true"},
			{"(>= 0.0 1)", "false"},
			{"(>= 0 1.0)", "false"},
			{"(>= 1 1)", "true"},
			{"(>= 1.0 1)", "true"},
			{"(>= 2 1)", "true"},
			{"(>= 2.0 1)", "true"},
		}},
		{"strings", TestSequence{
			{`(string= "" "")`, "true"},
			{`(string= "abc" "abc")`, "true"},
			{`(string= "abc" "ABC")`, `false`},
			{`(string= "abc" (format-string "ab{}" "c"))`, "true"},
			{`(string< "" "abc")`, "true"},
			{`(string< "ABC" "abc")`, "true"},
			{`(string< "a" "z")`, "true"},
			{`(string< "abc" "AABC")`, `false`},
			{`(string< "abc" "abc")`, `false`},
			{`(string<= "" "")`, "true"},
			{`(string<= "abc" "abc")`, "true"},
			{`(string<= "ABC" "abc")`, "true"},
			{`(string<= "abc" "abcdef")`, "true"},
			{`(string<= "abc" "")`, `false`},
			{`(string<= "abc" "ABC")`, `false`},
			{`(string> "abc" "ABC")`, "true"},
			{`(string> "abc" "")`, "true"},
			{`(string> "" "abc")`, `false`},
			{`(string> "" "")`, `false`},
			{`(string> "abc" "abc")`, `false`},
			{`(string>= "" "")`, "true"},
			{`(string>= "abc" "abc")`, "true"},
			{`(string>= "abc" "")`, "true"},
			{`(string>= "abc" "ABC")`, "true"},
			{`(string>= "ABC" "abc")`, `false`},
			{`(string>= "abc" "abcdef")`, `false`},
		}},
		{"equal?", TestSequence{
			{`(equal? () ())`, "true"},
			{`(equal? 2 2)`, "true"},
			{`(equal? 2 3)`, `false`},
			{`(equal? 2 (+ 1 1))`, "true"},
			{`(equal? 2 2.5)`, `false`},
			{`(equal? 2.1 2.5)`, `false`},
			{`(equal? 'a 'a)`, "true"},
			{`(equal? 'a 'b)`, `false`},
			{`(equal? "a" 'a)`, `false`},
			{`(equal? "a" "a")`, "true"},
			{`(equal? "a" "A")`, `false`},
		}},
		{"aggregation", TestSequence{
			{"(max 1)", "1"},
			{"(max 1 -1)", "1"},
			{"(max 1 -1 2 3 -1 0)", "3"},
			{"(min 1)", "1"},
			{"(min 1 -1)", "-1"},
			{"(min -3 -1 2 3 -1 0)", "-3"},
			{"(all? (expr (< % 3)) '(0 -1 2))", "true"},
			{"(all? (expr (< % 3)) '(0 -1 3 2))", "false"},
			{"(any? (expr (> % 3)) '(0 -1 2))", "false"},
			{"(any? (expr (> % 0)) '(0 -1 3 2))", "true"},
		}},
	}
	RunTestSuite(t, tests)
}
