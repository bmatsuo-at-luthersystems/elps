package langproc

import (
	"fmt"

	"github.com/luthersystems/elps/v2/pkg/environ"
	"github.com/luthersystems/elps/v2/pkg/lisp"
	"github.com/luthersystems/elps/v2/pkg/symbol"
)

var LPrimitiveProc = symbol.Intern("primitive-procedure")
var LCompoundProc = symbol.Intern("compound-procedure")

func Install(table symbol.Table, keepdocs bool, env *environ.Environ) {
	for name, defn := range LangProcs {
		proc := NewPrimitive(table, keepdocs, defn)
		env.Put(table.Intern(name), LVal(nil, LPrimitiveProc, proc))
	}
}

var LangProcs = map[string]Definition{
	"type":   ProcType,
	"cons":   ProcCons,
	"car":    ProcCAR,
	"cdr":    ProcCDR,
	"equal?": ProcEqual,
	"=":      ProcEQ,
	">":      ProcGT,
	">=":     ProcGTE,
	"<":      ProcLT,
	"<=":     ProcLTE,
	"+":      ProcAdd,
	"-":      ProcSub,
	"*":      ProcMul,
}

var ProcType = Simple(
	[]string{"v"},
	"Returns a symbol representing the type of v.",
	func(env Environ) lisp.LVal {
		v := env.GetAddress(0)
		return lisp.GetType(v)
	},
)

var ProcCons = Simple(
	[]string{"head", "tail"},
	"Returns a pair containing head and tail.",
	func(env Environ) lisp.LVal {
		head := env.GetAddress(0)
		tail := env.GetAddress(1)
		return lisp.Cons(head, tail)
	},
)

var ProcCAR = Simple(
	[]string{"p"},
	"Returns the left element of pair p.",
	func(env Environ) lisp.LVal {
		p := env.GetAddress(0)
		if lisp.IsNil(p) {
			return lisp.Nil()
		}
		cons, ok := lisp.GetCons(p)
		if !ok {
			return lisp.Error(fmt.Errorf("argument is not a list"))
		}
		return cons.CAR()
	},
)

var ProcCDR = Simple(
	[]string{"p"},
	"Returns the right element of pair p.",
	func(env Environ) lisp.LVal {
		p := env.GetAddress(0)
		if lisp.IsNil(p) {
			return lisp.Nil()
		}
		cons, ok := lisp.GetCons(p)
		if !ok {
			return lisp.Error(fmt.Errorf("argument is not a list"))
		}
		return cons.CDR()
	},
)

var ProcEqual = Simple(
	[]string{"v1", "v2"},
	"Returns true if v1 and v2 are identical",
	func(env Environ) lisp.LVal {
		lx := env.GetAddress(0)
		ly := env.GetAddress(1)
		return lisp.Bool(lisp.Equal(lx, ly))
	},
)

func numFloat(x lisp.LVal) (float64, bool) {
	if x, ok := lisp.GetFloat(x); ok {
		return x, true
	}
	if x, ok := lisp.GetInt(x); ok {
		return float64(x), true
	}
	return 0, false
}

func twoInts(x, y lisp.LVal) bool {
	return x.Type() == lisp.LInt && y.Type() == lisp.LInt
}

var ProcEQ = Simple(
	[]string{"x", "y"},
	"Returns true if numbers x and y are equal",
	func(env Environ) lisp.LVal {
		lx := env.GetAddress(0)
		ly := env.GetAddress(1)
		if twoInts(lx, ly) {
			return lisp.Bool(lx.Data == ly.Data)
		}
		x, ok := numFloat(lx)
		if !ok {
			return lisp.Error(fmt.Errorf("first argument is not an number"))
		}
		y, ok := numFloat(ly)
		if !ok {
			return lisp.Error(fmt.Errorf("second argument is not an number"))
		}
		return lisp.Bool(x == y)
	},
)

var ProcGT = Simple(
	[]string{"x", "y"},
	"Returns true if x is greater than y",
	func(env Environ) lisp.LVal {
		lx := env.GetAddress(0)
		ly := env.GetAddress(1)
		if twoInts(lx, ly) {
			return lisp.Bool(lx.Data > ly.Data)
		}
		x, ok := numFloat(lx)
		if !ok {
			return lisp.Error(fmt.Errorf("first argument is not an number"))
		}
		y, ok := numFloat(ly)
		if !ok {
			return lisp.Error(fmt.Errorf("second argument is not an number"))
		}
		return lisp.Bool(x > y)
	},
)

var ProcGTE = Simple(
	[]string{"x", "y"},
	"Returns true if x is greater than or equal to y",
	func(env Environ) lisp.LVal {
		lx := env.GetAddress(0)
		ly := env.GetAddress(1)
		if twoInts(lx, ly) {
			return lisp.Bool(lx.Data >= ly.Data)
		}
		x, ok := numFloat(lx)
		if !ok {
			return lisp.Error(fmt.Errorf("first argument is not an number"))
		}
		y, ok := numFloat(ly)
		if !ok {
			return lisp.Error(fmt.Errorf("second argument is not an number"))
		}
		return lisp.Bool(x >= y)
	},
)

var ProcLT = Simple(
	[]string{"x", "y"},
	"Returns true if x is less than y",
	func(env Environ) lisp.LVal {
		lx := env.GetAddress(0)
		ly := env.GetAddress(1)
		if twoInts(lx, ly) {
			return lisp.Bool(lx.Data < ly.Data)
		}
		x, ok := numFloat(lx)
		if !ok {
			return lisp.Error(fmt.Errorf("first argument is not an number"))
		}
		y, ok := numFloat(ly)
		if !ok {
			return lisp.Error(fmt.Errorf("second argument is not an number"))
		}
		return lisp.Bool(x < y)
	},
)

var ProcLTE = Simple(
	[]string{"x", "y"},
	"Returns true if x is less than or equal to y",
	func(env Environ) lisp.LVal {
		lx := env.GetAddress(0)
		ly := env.GetAddress(1)
		if twoInts(lx, ly) {
			return lisp.Bool(lx.Data <= ly.Data)
		}
		x, ok := numFloat(lx)
		if !ok {
			return lisp.Error(fmt.Errorf("first argument is not an number"))
		}
		y, ok := numFloat(ly)
		if !ok {
			return lisp.Error(fmt.Errorf("second argument is not an number"))
		}
		return lisp.Bool(x <= y)
	},
)

var ProcAdd = Simple(
	[]string{"&rest", "rest"},
	"Add numbers",
	func(env Environ) lisp.LVal {
		args := env.GetAddress(0)
		x := 0
		it := lisp.NewListIterator(args)
		for it.Next() {
			xi, ok := lisp.GetInt(it.Value())
			if !ok {
				return lisp.Error(fmt.Errorf("type is not a list"))
			}
			x += xi
		}
		if it.Err() != nil {
			return lisp.Error(it.Err())
		}
		return lisp.Int(x)
	},
)

var ProcSub = Simple(
	[]string{"&rest", "rest"},
	"Subtract numbers",
	func(env Environ) lisp.LVal {
		args := env.GetAddress(0)
		it := lisp.NewListIterator(args)
		var x int
		if it.Next() {
			xi, ok := lisp.GetInt(it.Value())
			if !ok {
				return lisp.Error(fmt.Errorf("argument is not an int"))
			}
			x = xi
		} else if it.Err() != nil {
			return lisp.Error(it.Err())
		} else {
			return lisp.Int(0)
		}
		if lisp.IsNil(it.Rest()) {
			return lisp.Int(-x)
		}
		for it.Next() {
			xi, ok := lisp.GetInt(it.Value())
			if !ok {
				return lisp.Error(fmt.Errorf("type is not a list"))
			}
			x -= xi
		}
		if it.Err() != nil {
			return lisp.Error(it.Err())
		}
		return lisp.Int(x)
	},
)

var ProcMul = Simple(
	[]string{"&rest", "rest"},
	"Multiply numbers",
	func(env Environ) lisp.LVal {
		args := env.GetAddress(0)
		x := 1
		it := lisp.NewListIterator(args)
		for it.Next() {
			xi, ok := lisp.GetInt(it.Value())
			if !ok {
				return lisp.Error(fmt.Errorf("type is not a list"))
			}
			x *= xi
		}
		if it.Err() != nil {
			return lisp.Error(it.Err())
		}
		return lisp.Int(x)
	},
)
