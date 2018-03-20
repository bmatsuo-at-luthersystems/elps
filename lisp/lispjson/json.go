package lispjson

import (
	"encoding/json"
	"fmt"

	"bitbucket.org/luthersystems/elps/lisp"
)

func init() {

}

// DefaultSerializer is the Serializer used by exported functions Load and
// Dump.
var DefaultSerializer = &Serializer{
	True:  lisp.Nil(),
	False: lisp.Nil(),
	Null:  lisp.Nil(),
}

// Dump serializes the structure of v as a JSON formatted byte slice.
func Dump(v *lisp.LVal) ([]byte, error) {
	return nil, fmt.Errorf("unimplemented")
}

// Load parses b as JSON and returns an equivalent LVal.
func Load(b []byte) *lisp.LVal {
	return DefaultSerializer.Load(b)
}

// Serializer defines JSON serialization rules for lisp values.
type Serializer struct {
	True  *lisp.LVal
	False *lisp.LVal
	Null  *lisp.LVal
}

// Load parses b and returns an LVal representing its structure.
func (s *Serializer) Load(b []byte) *lisp.LVal {
	var x interface{}
	err := json.Unmarshal(b, x)
	if err != nil {
		return lisp.Error(err)
	}
	return s.loadInterface(x)
}

func (s *Serializer) loadInterface(x interface{}) *lisp.LVal {
	if x == nil {
		return s.Null.Copy()
	}
	switch x := x.(type) {
	case bool:
		if x {
			return s.True.Copy()
		}
		return s.False.Copy()
	case string:
		return lisp.String(x)
	case float64:
		return lisp.Float(x)
	case map[string]interface{}:
		m := lisp.SortedMap()
		for k, v := range x {
			err := m.MapSet(k, s.loadInterface(v))
			if err.Type == lisp.LError {
				return err
			}
		}
		return m
	case []interface{}:
		lis := lisp.QExpr(make([]*lisp.LVal, len(x)))
		for i, v := range x {
			lis.Cells[i] = s.loadInterface(v)
			if lis.Cells[i].Type == lisp.LError {
				return lis.Cells[i]
			}
		}
		return lis
	default:
		return lisp.Errorf("unable to load json type: %T", x)
	}
}
