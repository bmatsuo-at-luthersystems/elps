package runtime

import (
	"fmt"
	"io"
	"os"
	"strings"

	"github.com/luthersystems/elps/v2/pkg/environ"
	"github.com/luthersystems/elps/v2/pkg/lisp"
	"github.com/luthersystems/elps/v2/pkg/symbol"
)

// Option is a function that configures a new Runtime.
type Option func(*Runtime) error

// WithStderr redirects a runtime's Stderr output stream to w instead of the
// default os.Stderr.
func WithStderr(w io.Writer) Option {
	return func(r *Runtime) error {
		if w == nil {
			return fmt.Errorf("nil stderr")
		}
		r.Stderr = w
		return nil
	}
}

// Runtime manages lisp execution for a virtual machine.  Runtime contains a
// symbol table and language-defined keywords.  Runtime has threads that can
// operate indepently.
type Runtime struct {
	Symbols  symbol.Table
	Bindings environ.Bindings
	Keywords map[symbol.ID]bool
	Stderr   io.Writer
	Threads  []*Thread
}

// New initializes and returns a new Runtime with the provided configuration
// options.  If any error is encountered it will be returned with a nil
// runtime.
func New(options ...Option) (*Runtime, error) {
	r := &Runtime{
		Symbols:  symbol.NewGlobalTable(),
		Bindings: environ.NewBindings(0),
		Keywords: make(map[symbol.ID]bool),
		Stderr:   os.Stderr,
	}
	for _, fn := range options {
		err := fn(r)
		if err != nil {
			return nil, err
		}
	}
	return r, nil
}

func (r *Runtime) NewThread() *Thread {
	return &Thread{
		Runtime:    r,
		Program:    nil,
		ParamStack: nil,
	}
}

// IsKeyword implements lisp.Runtime
func (r *Runtime) IsKeyword(id symbol.ID) bool {
	return r.Keywords[id]
}

// PutGlobal implements lisp.Runtime
func (r *Runtime) PutGlobal(id symbol.ID, v lisp.LVal) lisp.LVal {
	if r.Keywords[id] {
		name := r.symbol(id)
		if lisp.IsError(name) {
			return lisp.Error(fmt.Errorf("keyword without a name: %v", id))
		}
		return lisp.Error(fmt.Errorf("cannot bind a keyword: %v", name))
	}
	r.Bindings.Put(id, v)
	return lisp.Nil()
}

// GetGlobal implements lisp.Runtime
func (r *Runtime) GetGlobal(id symbol.ID) lisp.LVal {
	if r.Keywords[id] {
		return lisp.Symbol(id)
	}
	v, ok := r.Bindings.Get(id)
	if !ok {
		name := r.symbol(id)
		if lisp.IsError(name) {
			return name
		}
		return lisp.Error(fmt.Errorf("unbound symbol: %v", name))
	}
	return v
}

// Intern translates symobl to a symbol.ID and marks the symbol as a keyword
// when it starts with a colon ":".
func (r *Runtime) Intern(symbol string) symbol.ID {
	id := r.Symbols.Intern(symbol)
	if strings.HasPrefix(symbol, ":") {
		r.Keywords[id] = true
	}
	return id
}

// Symbol implements lisp.Runtime
func (r *Runtime) Symbol(id symbol.ID) (string, bool) {
	return r.Symbols.Symbol(id)
}

func (r *Runtime) symbol(id symbol.ID) lisp.LVal {
	name, ok := r.Symbol(id)
	if !ok {
		return lisp.Error(fmt.Errorf("symbol without a name: %v", id))
	}
	return lisp.String(name)
}
