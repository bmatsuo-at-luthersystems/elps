package mtest

import (
	"fmt"
	"testing"

	"github.com/luthersystems/elps/v2/pkg/asm"
	"github.com/luthersystems/elps/v2/pkg/machine"
	"github.com/luthersystems/elps/v2/pkg/symbol"
	"github.com/stretchr/testify/require"
)

// ThreadTraceFunc returns a machine.T.Trace func that logs every label and
// execution step performed by the thread.
func ThreadTraceFunc(t testing.TB, table symbol.Table) func([]symbol.ID, machine.Code) {
	return func(labels []symbol.ID, code machine.Code) {
		t.Helper()
		for _, label := range labels {
			t.Logf("LABEL %v", symbol.String(label, table))
		}
		if code.Text != nil {
			text, err := asm.InstructionString(code.Text, table)
			if err != nil {
				text = fmt.Sprintf("%#v", code.Text)
			}
			t.Logf("STEP %v", text)
		}
	}
}

// NewThread creates a new thread with m and installs a tracing function if
// trace is true.
func NewThread(t testing.TB, m *machine.M, trace bool) *machine.T {
	thd, err := m.NewThread()
	require.NoError(t, err, "Thread init error")
	if trace {
		table := m.Symbols
		if table == nil {
			table = symbol.DefaultGlobalTable
		}
		thd.Trace = ThreadTraceFunc(t, table)
	}
	return thd
}
