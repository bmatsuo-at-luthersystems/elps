package langproc

import (
	"fmt"

	"github.com/luthersystems/elps/v2/pkg/environ"
	"github.com/luthersystems/elps/v2/pkg/lisp"
	"github.com/luthersystems/elps/v2/pkg/symbol"
)

// Environ is an execution environment.  It is an abstraction of
// environ.Environ that also provides access to global runtime facilities like
// unified logging.
type Environ interface {
	Len() int                            // nargs
	GetAddress(i int) lisp.LVal          // positional args
	Get(symbol.ID) (lisp.LVal, bool)     // access package vars
	PutGlobal(id symbol.ID, v lisp.LVal) // set package vars
	Print(...interface{})                // log to "stderr"
}

// ProcFunc implements a primitive lisp procedure. env is a lexical environment
// with arguments accessible by name or through addressing into env.
//		func(env langproc.Environ) lisp.LVal {
//			x := env.GetAddress(0) // first arg
//			y := env.GetAddress(1) // second arg
//			// ...
//		}
type ProcFunc func(env Environ) lisp.LVal

// Definition defines is a primitive lisp procedure that is implemented as a go
// function.
type Definition interface {
	FormalArgs(table symbol.Table) lisp.LVal
	Documentation() string
	Implementation(table symbol.Table) ProcFunc
}

// SimpleDefn defines a basic primitive lisp procedure with a go function
// implementation.  SimpleDefn implements Definition.
type SimpleDefn struct {
	Args []string
	Docs string
	Fn   ProcFunc
}

func Simple(args []string, docs string, fn ProcFunc) *SimpleDefn {
	return &SimpleDefn{args, docs, fn}
}

// FormalArgs implements Definition.
func (d *SimpleDefn) FormalArgs(table symbol.Table) lisp.LVal {
	largs := []lisp.LVal{}
	for i := range d.Args {
		largs = append(largs, lisp.Symbol(table.Intern(d.Args[i])))
	}
	return lisp.ExprCompact(largs...)
}

// Documentation implements Definition.
func (d *SimpleDefn) Documentation() string {
	return d.Docs
}

// Implementation implements Definition.
func (d *SimpleDefn) Implementation(table symbol.Table) ProcFunc {
	return d.Fn
}

// Primitive is a callable lisp function that has been initialized for a
// machine, with its symbol table.
type Primitive struct {
	args lisp.LVal
	docs string
	fn   ProcFunc
}

// NewPrimitive creates a primitive lisp procedure given its definition.
func NewPrimitive(table symbol.Table, keepdocs bool, defn Definition) *Primitive {
	docs := ""
	if keepdocs {
		docs = defn.Documentation()
	}
	return &Primitive{defn.FormalArgs(table), docs, defn.Implementation(table)}
}

// FormalArgs returns a list of formal function parameters.  Depending on
// the language grammar for specifying arguments this may be more than a
// list of symbols.
func (proc *Primitive) FormalArgs() lisp.LVal {
	return proc.args
}

// Documentation is optional procedure documentation.
func (proc *Primitive) Documentation() string {
	return proc.docs
}

// Apply will execute the procedure using the given execution enviornment.
func (proc *Primitive) Apply(env Environ) lisp.LVal {
	return proc.fn(env)
}

// LVal constructs an LTaggedVal from proc with the given type
func LVal(source lisp.Source, typ symbol.ID, proc *Primitive) lisp.LVal {
	return lisp.TagNative(typ, proc)
}

// GetPrimitive extracts a Primitive from v.  GetPrimitive returns fals if v is
// not LTaggedVal or does not contain a Primitive.
func GetPrimitive(v lisp.LVal) (*Primitive, bool) {
	if v.Type() != lisp.LTaggedVal {
		return nil, false
	}
	p, ok := v.Native.(*Primitive)
	return p, ok
}

type Compound struct {
	formals lisp.LVal
	docs    lisp.LVal
	body    lisp.LVal
	env     *environ.Environ
}

func NewCompound(formals lisp.LVal, docs lisp.LVal, body lisp.LVal, env *environ.Environ) (*Compound, error) {
	it := lisp.NewListIterator(formals)
	for it.Next() {
		if it.Value().Type() != lisp.LSymbol {
			return nil, fmt.Errorf("formal argument is not a symbol")
		}
	}
	if it.Err() != nil {
		return nil, fmt.Errorf("formal arguments: %w", it.Err())
	}
	if !lisp.IsNil(docs) && docs.Type() != lisp.LString {
		return nil, fmt.Errorf("invalid documentation")
	}
	it = lisp.NewListIterator(body)
	for it.Next() {
	}
	if it.Err() != nil {
		return nil, fmt.Errorf("body: %w", it.Err())
	}
	proc := &Compound{
		formals: formals,
		docs:    docs,
		body:    body,
		env:     env,
	}
	return proc, nil
}

func (proc *Compound) FormalArgs() lisp.LVal {
	return proc.formals
}

// Documentation is optional procedure documentation.
func (proc *Compound) Documentation() lisp.LVal {
	return proc.docs
}

func (proc *Compound) Body() lisp.LVal {
	return proc.body
}

func (proc *Compound) Environ() *environ.Environ {
	return proc.env
}

func (proc *Compound) LVal(source lisp.Source, typ symbol.ID) lisp.LVal {
	return lisp.TagNative(typ, proc)
}

func GetCompound(v lisp.LVal) (*Compound, bool) {
	if v.Type() != lisp.LTaggedVal {
		return nil, false
	}
	proc, ok := v.Native.(*Compound)
	if !ok {
		return nil, false
	}
	return proc, true
}
