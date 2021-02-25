package environ

import (
	"fmt"

	"github.com/luthersystems/elps/v2/pkg/internal/lisputil"
	"github.com/luthersystems/elps/v2/pkg/lisp"
	"github.com/luthersystems/elps/v2/pkg/symbol"
)

// Bindings is a set of variable bindings (e.g. function arguments).
type Bindings interface {
	// Len returns the number of variables bound
	Len() int
	// Get returns the value bound to the given symbol.
	Get(symbol.ID) (lisp.LVal, bool)
	// Put creates or updates a binding for the given symbol with the given
	// value.
	Put(symbol.ID, lisp.LVal)
}

// OrderedBindings are Bindings that support integer indexing for variables.
type OrderedBindings interface {
	// LexicalBindings are Bindings
	Bindings
	// GetIndex returns the value bound to the variable at the given index.
	GetIndex(int) lisp.LVal
	// PutIndex updates the variable at the given index with the given value.
	PutIndex(int, lisp.LVal)
}

// NewBindings creates and initializes a new set of variable bindings that has
// initial capacity to hold n values.
func NewBindings(n int) Bindings {
	return newBindings(n)
}

type bindingPair struct {
	name  symbol.ID
	value lisp.LVal
}

// Bindings is a set of lexical variable bindings.
type bindings struct {
	pairs []bindingPair
	index map[symbol.ID]int
}

var _ OrderedBindings = (*bindings)(nil)

func newBindings(n int) *bindings {
	return &bindings{
		pairs: make([]bindingPair, 0, n),
		index: make(map[symbol.ID]int, n),
	}
}

// NewBindingsZipCons takes a list of variable names with a list of variable
// values and returns the corresponding bindings.  If vars and vars are not
// lists of equal length newBindingsZipCons returns an error.
func NewBindingsZipCons(vars, vals lisp.LVal) (Bindings, error) {
	n, ok := lisputil.ConsLen(vars)
	if !ok {
		return nil, fmt.Errorf("variable list: not a list")
	}
	s := newBindings(n)
	itVars := lisp.NewListIterator(vars)
	itVals := lisp.NewListIterator(vals)
	for itVars.Next() {
		if !itVals.Next() {
			if itVals.Err() != nil {
				return nil, fmt.Errorf("value list: %w", itVals.Err())
			}
			return nil, fmt.Errorf("variable and value lists have unequal lengths")
		}
		name, ok := lisp.GetSymbol(itVars.Value())
		if !ok {
			return nil, fmt.Errorf("variable is not a symbol: %v", itVars.Value().Type())
		}
		s.Put(name, itVals.Value())
	}
	if itVars.Err() != nil {
		return nil, fmt.Errorf("variable list: %w", itVars.Err())
	}
	if !lisp.IsNil(itVals.Rest()) {
		return nil, fmt.Errorf("variable and value lists have unequal lengths")
	}
	return s, nil
}

// Len returns the number of symbols bound.
func (s *bindings) Len() int {
	return len(s.pairs)
}

// GetSymbol returns the variable at index i.
func (s *bindings) GetVariable(i int) symbol.ID {
	return s.pairs[i].name
}

// GetSymbol returns the value at index i.
func (s *bindings) GetIndex(i int) lisp.LVal {
	return s.pairs[i].value
}

// Get returns the value bound to variable.
func (s *bindings) Get(variable symbol.ID) (lisp.LVal, bool) {
	i, ok := s.index[variable]
	if !ok {
		return lisp.Nil(), false
	}
	return s.pairs[i].value, true
}

// PutIndex rebinds the variable at index i to v.
func (s *bindings) PutIndex(i int, v lisp.LVal) {
	s.pairs[i].value = v
}

// Put binds variable to v.  If variable was previously bound its entry will be
// updated.  Otherwise Put creates a new variable binding.
func (s *bindings) Put(variable symbol.ID, v lisp.LVal) {
	i, ok := s.index[variable]
	if ok {
		s.PutIndex(i, v)
		return
	}
	s.index[variable] = len(s.pairs)
	s.pairs = append(s.pairs, bindingPair{variable, v})
}
