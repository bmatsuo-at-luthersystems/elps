package libjson

import (
	"encoding/json"
	"fmt"
	"reflect"

	"bitbucket.org/luthersystems/elps/lisp"
	"bitbucket.org/luthersystems/elps/lisp/lisplib/internal/libutil"
)

func init() {
	DefaultSerializer = &Serializer{
		True:  lisp.Symbol("json:true"),
		False: lisp.Symbol("json:false"),
		Null:  lisp.Symbol("json:null"),
	}
}

// DeafultPackageName is the package name used by LoadPackage.
const DefaultPackageName = "json"

// LoadPackage adds the time package to env
func LoadPackage(env *lisp.LEnv) *lisp.LVal {
	name := lisp.Symbol(DefaultPackageName)
	e := env.DefinePackage(name)
	if !e.IsNil() {
		return e
	}
	e = env.InPackage(name)
	if !e.IsNil() {
		return e
	}
	env.PutGlobal(lisp.Symbol("null"), lisp.Symbol("json:null"))
	env.PutGlobal(lisp.Symbol("true"), lisp.Symbol("json:true"))
	env.PutGlobal(lisp.Symbol("false"), lisp.Symbol("json:false"))
	for _, fn := range Builtins(DefaultSerializer) {
		env.AddBuiltins(true, fn)
	}
	return lisp.Nil()
}

// Bulitins takes the default serializer for a lisp environment and returns a
// set of package builtin functions that use it.
func Builtins(s *Serializer) []*libutil.Builtin {
	return []*libutil.Builtin{
		libutil.Function("dump-bytes", lisp.Formals("object"), s.DumpBytesBuiltin),
		libutil.Function("load-bytes", lisp.Formals("object"), s.LoadBytesBuiltin),
		libutil.Function("dump-string", lisp.Formals("object"), s.DumpStringBuiltin),
		libutil.Function("load-string", lisp.Formals("json-string"), s.LoadStringBuiltin),
	}
}

// DefaultSerializer is the Serializer used by exported functions Load and
// Dump.
var DefaultSerializer *Serializer

// Dump serializes the structure of v as a JSON formatted byte slice.
func Dump(v *lisp.LVal) ([]byte, error) {
	return DefaultSerializer.Dump(v)
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
	err := json.Unmarshal(b, &x)
	if err != nil {
		return lisp.Error(err)
	}
	return s.loadInterface(x)
}

func (s *Serializer) loadInterface(x interface{}) *lisp.LVal {
	if x == nil {
		return lisp.Nil()
	}
	switch x := x.(type) {
	case bool:
		if x {
			return lisp.Symbol("t")
		}
		return lisp.Nil()
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

// Dump serializes v as JSON and returns any error.
func (s *Serializer) Dump(v *lisp.LVal) ([]byte, error) {
	m := s.GoValue(v)
	_, badnews := m.(*lisp.LVal)
	if badnews {
		return nil, fmt.Errorf("type cannot be converted to json: %v", v.Type)
	}
	return json.Marshal(m)
}

func (s *Serializer) DumpBytesBuiltin(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	obj := args.Cells[0]
	b, err := s.Dump(obj)
	if err != nil {
		return env.Error(err)
	}
	return lisp.Bytes(b)
}

func (s *Serializer) LoadBytesBuiltin(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	js := args.Cells[0]
	if js.Type != lisp.LBytes {
		return env.Errorf("argument is not bytes: %v", js.Type)
	}
	return s.Load(js.Bytes)
}

func (s *Serializer) DumpStringBuiltin(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	obj := args.Cells[0]
	b, err := s.Dump(obj)
	if err != nil {
		return env.Error(err)
	}
	return lisp.String(string(b))
}

func (s *Serializer) LoadStringBuiltin(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	js := args.Cells[0]
	if js.Type != lisp.LString {
		return env.Errorf("argument is not a string: %v", js.Type)
	}
	return s.Load([]byte(js.Str))
}

// GoValue converts v to its natural representation in Go.  Quotes are ignored
// and all lists are turned into slices.  Symbols are converted to strings.
// The value Nil() is converted to nil.  Functions are returned as is.
func (s *Serializer) GoValue(v *lisp.LVal) interface{} {
	if v.IsNil() {
		return nil
	}
	switch v.Type {
	case lisp.LError:
		return (error)((*lisp.ErrorVal)(v))
	case lisp.LSymbol, lisp.LString:
		if v.Type == lisp.LSymbol {
			switch v.Str {
			case "t":
				return true
			case s.True.Str:
				return true
			case s.False.Str:
				return false
			case s.Null.Str:
				return nil
			}
		}
		return v.Str
	case lisp.LInt:
		return v.Int
	case lisp.LFloat:
		return v.Float
	case lisp.LQuote:
		return s.GoValue(v.Cells[0])
	case lisp.LSExpr:
		s, _ := s.GoSlice(v)
		return s
	case lisp.LSortMap:
		m, _ := s.GoMap(v)
		return m
	}
	return v
}

// GoError returns an error that represents v.  If v is not LError then nil is
// returned.
func (s *Serializer) GoError(v *lisp.LVal) error {
	if v.Type != lisp.LError {
		return nil
	}
	return (*lisp.ErrorVal)(v)
}

// GoString returns the string that v represents and the value true.  If v does
// not represent a string GoString returns a false second argument
func (s *Serializer) GoString(v *lisp.LVal) (string, bool) {
	if v.Type != lisp.LString {
		return "", false
	}
	return v.Str, true
}

// SymbolName returns the name of the symbol that v represents and the value
// true.  If v does not represent a symbol SymbolName returns a false second
// argument
func (s *Serializer) SymbolName(v *lisp.LVal) (string, bool) {
	if v.Type != lisp.LSymbol {
		return "", false
	}
	return v.Str, true
}

// GoInt converts the numeric value that v represents to and int and returns it
// with the value true.  If v does not represent a number GoInt returns a
// false second argument
func (s *Serializer) GoInt(v *lisp.LVal) (int, bool) {
	if v.IsNumeric() {
		return 0, false
	}
	if v.Type == lisp.LFloat {
		return int(v.Float), true
	}
	return v.Int, true
}

// GoFloat64 converts the numeric value that v represents to a float64 and
// returns it with the value true.  If v does not represent a number GoFloat64
// returns a false second argument
func (s *Serializer) GoFloat64(v *lisp.LVal) (float64, bool) {
	if v.IsNumeric() {
		return 0, false
	}
	if v.Type == lisp.LFloat {
		return v.Float, true
	}
	return float64(v.Int), true
}

// GoSlice returns the string that v represents and the value true.  If v does
// not represent a string GoSlice returns a false second argument
func (s *Serializer) GoSlice(v *lisp.LVal) ([]interface{}, bool) {
	if v.Type != lisp.LSExpr {
		return nil, false
	}
	vs := make([]interface{}, len(v.Cells))
	for i := range vs {
		vs[i] = s.GoValue(v.Cells[i])
	}
	return vs, true
}

// GoMap converts an LSortMap to its Go equivalent and returns it with a true
// second argument.  If v does not represent a map json serializable map GoMap
// returns a false second argument
func (s *Serializer) GoMap(v *lisp.LVal) (map[string]interface{}, bool) {
	if v.Type != lisp.LSortMap {
		return nil, false
	}
	m := make(map[string]interface{}, len(v.Map))
	for k, vlisp := range v.Map {
		vgo := s.GoValue(vlisp)
		kreflect := reflect.ValueOf(k)
		// This is really shitty
		switch kreflect.Kind() {
		case reflect.String:
			m[kreflect.String()] = vgo
		default:
			return nil, false
		}
	}
	return m, true
}
