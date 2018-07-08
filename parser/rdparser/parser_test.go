package rdparser

import (
	"fmt"
	"strings"
	"testing"

	"bitbucket.org/luthersystems/elps/parser/token"
)

func TestComments(t *testing.T) {
	tests := []struct {
		source string
		output string
	}{
		{`(1 2 3) ; A comment`, `(1 2 3)`},
		{`	; A comment
			(1 "abc" '(x y z))`, `(1 "abc" '(x y z))`},
		{`(1 "abc" ; A comment
			'(x y z))`, `(1 "abc" '(x y z))`},
	}

	for i, test := range tests {
		name := fmt.Sprintf("test%d", i)
		p := New(token.NewScanner(name, strings.NewReader(test.source)))
		exprs, err := p.ParseProgram()
		if err != nil {
			t.Errorf("test %d: parse error: %v", i, err)
			continue
		}
		for _, expr := range exprs {
			t.Log(expr)
		}
		if len(exprs) != 1 {
			t.Errorf("test %d: parsed %d expressions", i, len(exprs))
			continue
		}
		if exprs[0].String() != test.output {
			t.Errorf("test %d: expected output: %s", i, test.output)
		}
	}
}

func TestParser(t *testing.T) {
	tests := []struct {
		source string
		output string
	}{
		{`0`, `0`},
		{`12`, `12`},
		{`0.3`, `0.3`},
		{`-1`, `-1`},
		{`abc`, `abc`},
		{`abc?`, `abc?`},
		{`xyz:abc?`, `xyz:abc?`},
		{`x`, `x`},
		{`'xyz`, `'xyz`},
		{`"xyz"`, `"xyz"`},
		{`"x\nyz"`, `"x\nyz"`},
		{`"x\tyz"`, `"x\tyz"`},
		{`"x	yz"`, `"x\tyz"`},
		{`""`, `""`},
		{`""""""`, `""`},
		{`"""\n"""`, `"\\n"`},
		{`()`, `()`},
		{`'()`, `'()`},
		{`(1 2 3)`, `(1 2 3)`},
		{`(1 "abc" '(x y z))`, `(1 "abc" '(x y z))`},
		{`(1 "abc" [x y z])`, `(1 "abc" '(x y z))`},
		{`#^"abc"`, `(lisp:expr "abc")`},
		{`#^'%`, `(lisp:expr '%)`},
		{`#^%1`, `(lisp:expr %1)`},
		{`#^"abc"`, `(lisp:expr "abc")`},
		{`#^12.25`, `(lisp:expr 12.25)`},
		{`#^()`, `(lisp:expr ())`},
		{`#^(cons %1 %&rest)`, `(lisp:expr (cons %1 %&rest))`},
	}

	for i, test := range tests {
		name := fmt.Sprintf("test%d", i)
		p := New(token.NewScanner(name, strings.NewReader(test.source)))
		exprs, err := p.ParseProgram()
		if err != nil {
			t.Errorf("test %d: parse error: %v", i, err)
			continue
		}
		for _, expr := range exprs {
			t.Log(expr)
		}
		if len(exprs) != 1 {
			t.Errorf("test %d: parsed %d expressions", i, len(exprs))
			continue
		}
		if exprs[0].String() != test.output {
			t.Errorf("test %d: expected output: %s", i, test.output)
		}
	}
}
