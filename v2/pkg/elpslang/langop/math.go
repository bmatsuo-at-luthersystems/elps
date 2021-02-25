package langop

import (
	"fmt"

	"github.com/luthersystems/elps/v2/pkg/lisp"
)

// The math operators are designed with several ideas in mind.
//		- They are not general purpose because the lisp procedures will work
//		with a cons list which will have a different iteration construct.  So
//		it is OK to operate only on LInt values.
//		- The machine does not need  to concern itself with combining a list of
//		numbers, concerning itself with operator identity, etc.  Such an
//		operator can be constructed out of a logical constructs and a binary
//		operator if desired.  So only a binary operator needs to be provided as
//		the machine will provide adequate logical costructs.
//		- Failure of a machine program to handle preconditions like LInt
//		arguments or division by 0 are a failure of the code and will halt
//		execution.
// TODO: consider whether type checking the values is is worth it for these
// math operators

// EQ returns true iff v[0] = v[1] for two LInt arguments.  EQ returns LError
// if not called with two arguments or if either argument is not LInt.
func EQ(v ...lisp.LVal) (lisp.LVal, error) {
	x1, x2, err := binaryInt(v)
	return lisp.Bool(x1 == x2), err
}

// LT returns true iff v[0] < v[1] for two LInt arguments.  LT returns LError
// if not called with two arguments or if either argument is not LInt.
func LT(v ...lisp.LVal) (lisp.LVal, error) {
	x1, x2, err := binaryInt(v)
	return lisp.Bool(x1 < x2), err
}

// LTE returns true iff v[0] <= v[1] for two LInt arguments.  LTE returns LError
// if not called with two arguments or if either argument is not LInt.
func LTE(v ...lisp.LVal) (lisp.LVal, error) {
	x1, x2, err := binaryInt(v)
	return lisp.Bool(x1 <= x2), err
}

// GT returns true iff v[0] > v[1] for two LInt arguments.  GT returns LError
// if not called with two arguments or if either argument is not LInt.
func GT(v ...lisp.LVal) (lisp.LVal, error) {
	x1, x2, err := binaryInt(v)
	return lisp.Bool(x1 > x2), err
}

// GTE returns true iff v[0] >= v[1] for two LInt arguments.  GTT returns
// LError if not called with two arguments or if either argument is not LInt.
func GTE(v ...lisp.LVal) (lisp.LVal, error) {
	x1, x2, err := binaryInt(v)
	return lisp.Bool(x1 >= x2), err
}

// Add computes the sum of two LInt arguments, v[0] + v[1].
func Add(v ...lisp.LVal) (lisp.LVal, error) {
	x1, x2, err := binaryInt(v)
	return lisp.Int(x1 + x2), err
}

// Sub computes the difference of two LInt arguments, v[0] - v[1].
func Sub(v ...lisp.LVal) (lisp.LVal, error) {
	x1, x2, err := binaryInt(v)
	return lisp.Int(x1 - x2), err
}

// Mul computes the product of two LInt arguments, v[0] * v[1].
func Mul(v ...lisp.LVal) (lisp.LVal, error) {
	x1, x2, err := binaryInt(v)
	return lisp.Int(x1 * x2), err
}

// Div returns the quotient of two LInt arguments, v[0] / v[1].
func Div(v ...lisp.LVal) (lisp.LVal, error) {
	x1, x2, err := binaryInt(v)
	if err != nil {
		return lisp.Int(0), err
	}
	if x2 == 0 {
		return lisp.Int(0), fmt.Errorf("integer division by 0")
	}
	return lisp.Int(x1 / x2), nil
}

// Mod returns the modulo of two LInt arguments, v[0] % v[1].
func Mod(v ...lisp.LVal) (lisp.LVal, error) {
	x1, x2, err := binaryInt(v)
	if err != nil {
		return lisp.Int(0), err
	}
	if x2 == 0 {
		return lisp.Int(0), fmt.Errorf("integer division by 0")
	}
	return lisp.Int(x1 % x2), nil
}

func binaryInt(v []lisp.LVal) (int, int, error) {
	if len(v) != 2 {
		return 0, 0, fmt.Errorf("two arguments are required: %v", v)
	}
	x1, ok := lisp.GetInt(v[0])
	if !ok {
		return 0, 0, fmt.Errorf("first argument is not an integer")
	}
	x2, ok := lisp.GetInt(v[1])
	if !ok {
		return 0, 0, fmt.Errorf("second argument is not an integer")
	}
	return x1, x2, nil
}
