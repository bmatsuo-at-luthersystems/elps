package registertest

import (
	"fmt"
	"strings"
	"testing"

	"github.com/luthersystems/elps/v2/pkg/lisp"
	"github.com/luthersystems/elps/v2/pkg/symbol"
)

// TraceFunc returns a elpslang.Register.Trace func that logs the old
// and new register value for every call to SetValue.
func TraceFunc(t testing.TB, table symbol.Table) func(symbol.ID, lisp.LVal, lisp.LVal) {
	if table == nil {
		table = symbol.DefaultGlobalTable
	}
	return func(name symbol.ID, oldval lisp.LVal, newval lisp.LVal) {
		t.Helper()
		t.Logf("REGISTER %s: old=%v new=%v",
			symbol.String(name, table), valString(t, oldval, table), valString(t, newval, table))
	}
}

func valString(t testing.TB, v lisp.LVal, table symbol.Table) string {
	w := &strings.Builder{}
	_, err := lisp.Format(w, v, symbol.ResolveUnknown("#<UNKNOWN-SYMBOL %#x>", table))
	if err != nil {
		t.Logf("unable to format lval string: %v", err)
		return fmt.Sprintf("%#v", v)
	}
	return w.String()
}
