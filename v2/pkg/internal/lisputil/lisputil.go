// Package lisputil contains utility functions that aren't essential enough to
// be included in package lisp.
package lisputil

import "github.com/luthersystems/elps/v2/pkg/lisp"

// ConsLen performs an efficient iteration of list v to compute its length.
// ConsLen returns false if v is not a list.
func ConsLen(v lisp.LVal) (int, bool) {
	n := 0
	for !lisp.IsNil(v) {
		if v.Type() == lisp.LQuote {
			v = v.Native.(lisp.LVal)
		}
		if v.Type() != lisp.LCons {
			return n, false
		}
		v = v.Native.(*lisp.ConsData).CDR
		n++
	}
	return n, true
}
