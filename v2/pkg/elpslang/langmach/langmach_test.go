package langmach

import (
	"testing"

	"github.com/luthersystems/elps/v2/pkg/elpslang/langarch"
	"github.com/luthersystems/elps/v2/pkg/elpslang/langop"
	"github.com/luthersystems/elps/v2/pkg/elpslang/langproc"
	"github.com/luthersystems/elps/v2/pkg/elpslang/stack"
	"github.com/luthersystems/elps/v2/pkg/environ"
	"github.com/luthersystems/elps/v2/pkg/internal/bootstrapparser"
	"github.com/luthersystems/elps/v2/pkg/lisp"
	"github.com/luthersystems/elps/v2/pkg/machine/mtest"
	"github.com/luthersystems/elps/v2/pkg/register/registertest"
	"github.com/luthersystems/elps/v2/pkg/rmach"
	"github.com/luthersystems/elps/v2/pkg/symbol"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

type printerFunc func(...interface{})

func (fn printerFunc) Print(v ...interface{}) {
	fn(v...)
}

func testPrinter(t testing.TB) langop.Printer {
	return printerFunc(t.Log)
}

func TestMachine(t *testing.T) {
	table := symbol.CopyGlobalTable()
	global := environ.New(nil, nil)
	langproc.Install(table, true, global)
	m, err := NewMachine(testPrinter(t), table, global)
	assert.NoError(t, err)
	assert.NotNil(t, m)
	program, err := bootstrapparser.ParseProgramString(table.Intern("main.lisp"), table, `(+ 1 2 3 4)`)
	require.NoError(t, err)
	rtrace := registertest.TraceFunc(t, table)
	v, err := Evaluate(m, program,
		WithThreadTraceFunc(mtest.ThreadTraceFunc(t, table)),
		WithRegisterTraceFunc(langarch.RUnev, rtrace),
		WithRegisterTraceFunc(langarch.RExp, rtrace),
	)
	if assert.NoError(t, err) {
		x, ok := lisp.GetInt(v)
		if assert.True(t, ok) {
			assert.Equal(t, 10, x)
		}
	}
}

func TestMachineFact(t *testing.T) {
	table := symbol.CopyGlobalTable()
	global := environ.New(nil, nil)
	langproc.Install(table, true, global)
	m, err := NewMachine(testPrinter(t), table, global)
	assert.NoError(t, err)
	assert.NotNil(t, m)
	program, err := bootstrapparser.ParseProgramString(table.Intern("main.lisp"), table,
		`(set fact (lambda (n) (if (< n 2) n (* n (fact (- n 1))))))
		(fact 6)`)
	require.NoError(t, err)
	v, err := Evaluate(m, program)
	if assert.NoError(t, err) {
		x, ok := lisp.GetInt(v)
		if assert.True(t, ok) {
			assert.Equal(t, 1*2*3*4*5*6, x)
		}
	}
}

func TestMachineFact2(t *testing.T) {
	table := symbol.CopyGlobalTable()
	global := environ.New(nil, nil)
	langproc.Install(table, true, global)
	m, err := NewMachine(testPrinter(t), table, global)
	assert.NoError(t, err)
	assert.NotNil(t, m)
	program, err := bootstrapparser.ParseProgramString(table.Intern("main.lisp"), table,
		`(set fact-tailrec (lambda (acc n) (if (< n 2) acc (fact-tailrec (* n acc) (- n 1)))))
		(set fact (lambda (n) (fact-tailrec 1 n)))
		(fact 6)`)
	require.NoError(t, err)
	v, err := Evaluate(m, program)
	if assert.NoError(t, err) {
		x, ok := lisp.GetInt(v)
		if assert.True(t, ok) {
			assert.Equal(t, 1*2*3*4*5*6, x)
		}
	}
}

func TestMachineApply(t *testing.T) {
	table := symbol.CopyGlobalTable()
	global := environ.New(nil, nil)
	langproc.Install(table, true, global)
	m, err := NewMachine(testPrinter(t), table, global)
	assert.NoError(t, err)
	assert.NotNil(t, m)
	program, err := bootstrapparser.ParseProgramString(table.Intern("main.lisp"), table,
		`(set fact-tailrec (lambda (acc n) (if (< n 2) acc (fact-tailrec (* n acc) (- n 1)))))
		(set fact (lambda (n) (fact-tailrec 1 n)))`)
	require.NoError(t, err)
	_, err = Evaluate(m, program)
	require.NoError(t, err)
	thd, err := NewThread(m) //, WithThreadTraceFunc(mtest.ThreadTraceFunc(t, table)))
	require.NoError(t, err)
	defer thd.Stop(false)
	rstack, err := rmach.GetThreadRegister(thd, table.Intern("stack"))
	require.NoError(t, err)
	fact := table.Intern("fact")
	for i := 0; i < 5; i++ {
		v, err := Apply(thd, lisp.Symbol(fact), lisp.List(nil, lisp.Int(4)))
		if assert.NoError(t, err) {
			x, ok := lisp.GetInt(v)
			if assert.True(t, ok) {
				assert.Equal(t, 24, x)
			}
		}
		s, ok := stack.GetStack(rstack.GetValue())
		if assert.True(t, ok) {
			assert.Equal(t, 0, s.Len())
		}
	}
}

func BenchmarkMachineFact(b *testing.B) {
	table := symbol.CopyGlobalTable()
	global := environ.New(nil, nil)
	langproc.Install(table, true, global)
	m, err := NewMachine(testPrinter(b), table, global)
	assert.NoError(b, err)
	assert.NotNil(b, m)
	program, err := bootstrapparser.ParseProgramString(table.Intern("main.lisp"), table,
		`(set fact-tailrec (lambda (acc n) (if (< n 2) acc (fact-tailrec (* n acc) (- n 1)))))
		(set fact (lambda (n) (fact-tailrec 1 n)))`)
	require.NoError(b, err)
	_, err = Evaluate(m, program)
	require.NoError(b, err)
	thd, err := m.NewThread()
	require.NoError(b, err)
	defer thd.Stop(false)
	b.ResetTimer()
	fact := table.Intern("fact")
	for i := 0; i < b.N; i++ {
		v, err := Apply(thd, lisp.Symbol(fact), lisp.List(nil, lisp.Int(20)))
		if assert.NoError(b, err) {
			x, ok := lisp.GetInt(v)
			if assert.True(b, ok) {
				assert.Equal(b, 2432902008176640000, x)
			}
		}
	}
}
