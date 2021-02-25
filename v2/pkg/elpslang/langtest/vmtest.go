package vmtest

import (
	"testing"

	"github.com/luthersystems/elps/v2/pkg/elpslang"
	"github.com/luthersystems/elps/v2/pkg/elpslang/langasm"
	"github.com/luthersystems/elps/v2/pkg/machine"
	"github.com/luthersystems/elps/v2/pkg/machine/mtest"
	"github.com/luthersystems/elps/v2/pkg/register/registertest"
	"github.com/luthersystems/elps/v2/pkg/symbol"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

/*
// NewVM initializes a new vm with the given assembler and returns.  If vm
// creation fails NewVM will fail the test immediately.
func NewVM(t testing.TB) *elpslang.VM {
	t.Helper()
	vm, err := elpslang.NewVM(langasm.NewAssembler())
	require.NoError(t, err, "VM init error")
	return vm
}
*/

// NewThread creates a new thread with m and installs a tracing function if
// trace is true.
func NewThread(t testing.TB, m *elpslang.VM, trace bool) *machine.T {
	t.Helper()
	thd, err := m.NewThread()
	require.NoError(t, err, "Thread init error")
	if trace {
		table := m.M.Symbols
		if table == nil {
			table = symbol.DefaultGlobalTable
		}
		thd.Trace = mtest.ThreadTraceFunc(t, table)
	}
	return thd
}

// TraceOnThreadRegister enables tracing on the specified langasm thread
// register.
//		TraceOnThreadRegister(t, thd, langasm.RVal)
func TraceOnThreadRegister(t testing.TB, thd *machine.T, regid symbol.ID) {
	register, err := langasm.GetThreadRegister(thd, regid)
	if assert.NoError(t, err, "Unable to retreive register") {
		register.Trace = registertest.TraceFunc(t, thd.M().Symbols)
	}
}

// TraceOffThreadRegister disables tracing on the specified langasm thread
// register.
//		TraceOffThreadRegister(t, thd, langasm.RContinue)
func TraceOffThreadRegister(t testing.TB, thd *machine.T, regid symbol.ID) {
	register, err := langasm.GetThreadRegister(thd, regid)
	if assert.NoError(t, err, "Unable to retreive register") {
		register.Trace = nil
	}
}
