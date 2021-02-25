package langasm

import (
	"fmt"
	"testing"

	"github.com/luthersystems/elps/v2/pkg/elpslang/langarch"
	"github.com/luthersystems/elps/v2/pkg/elpslang/langop"
	"github.com/luthersystems/elps/v2/pkg/elpslang/stack"
	"github.com/luthersystems/elps/v2/pkg/internal/bootstrapparser"
	"github.com/luthersystems/elps/v2/pkg/lisp"
	"github.com/luthersystems/elps/v2/pkg/machine/mtest"
	"github.com/luthersystems/elps/v2/pkg/register"
	"github.com/luthersystems/elps/v2/pkg/register/registertest"
	"github.com/luthersystems/elps/v2/pkg/rmach"
	"github.com/luthersystems/elps/v2/pkg/symbol"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// The following example is modified from Structure and Interpretation of
// Computer Programs.
var (
	factRegN       = symbol.Intern("n")
	factThreadArch = register.MustArch(
		langarch.RStack,
		langarch.RContinue,
		langarch.RCondition,
		langarch.RFlag,
		langarch.RVal,
		factRegN,
	)
	factProgram = `
			(assign (reg continue) (label fact-done))
		fact-loop
			(test (op <=) (reg n) (const 1))
			(branch (label base-case))
			;; Set up for the recursive call by saving n and continue.
			;; Set up continue so that the computation will continue
			;; at after-fact when the subroutine returns.
			(save (reg continue))
			(save (reg n))
			(assign (reg n) (op -) (reg n) (const 1))
			(assign (reg continue) (label after-fact))
			(goto (label fact-loop))
		after-fact
			(restore (reg n))
			(restore (reg continue))
			(assign (reg val) (op *) (reg n) (reg val))
			(goto (reg continue))
		base-case
			(assign (reg val) (const 1))
			(goto (reg continue))
		fact-done
	`
)

type factMachine struct {
	rmach.Machine
	rmach.Assembler
	testing testing.TB
}

var factArch = rmach.MustArch(register.MustArch(), factThreadArch,
	rmach.WithThreadInit(func(thd rmach.Thread) error {
		r, ok := register.Lookup(thd, langarch.RStack)
		if !ok {
			return fmt.Errorf("unable to find the %v register", langarch.RStack)
		}
		r.SetValue(stack.New().LVal(nil, langarch.RStack))
		return nil
	}))

func newFactMachine(t testing.TB) (*factMachine, error) {
	m, err := rmach.NewMachine(factArch, nil, langop.Ops)
	if err != nil {
		return nil, err
	}
	fact := &factMachine{
		Machine:   m,
		Assembler: NewAssembler(factArch),
		testing:   t,
	}
	pname := fact.M().Symbols.Intern("main")
	program, err := bootstrapparser.ParseProgramString(pname, fact.M().Symbols, factProgram)
	if err != nil {
		return nil, fmt.Errorf("program parse error: %w", err)
	}
	obj, err := fact.Assemble(program)
	if err != nil {
		return nil, err
	}
	err = obj.Link(fact, pname)
	if err != nil {
		return nil, err
	}
	return fact, nil
}

func (m *factMachine) Fact(n int, trace bool) (int, error) {
	t, err := m.NewThread()
	if err != nil {
		return 0, err
	}
	defer t.Stop(false)
	if trace {
		m.testing.Logf("NUMSTEP %d", t.NumStep())
		t.Trace = mtest.ThreadTraceFunc(m.testing, m.M().Symbols)
	}
	thd, ok := rmach.GetThread(t)
	if !ok {
		return 0, fmt.Errorf("machine thread data is not rmach.Thread: %T", t.Data)
	}
	rn, ok := register.Lookup(thd, factRegN)
	if !ok {
		return 0, fmt.Errorf("failed to find the %s register", factRegN)
	}
	rval, ok := register.Lookup(thd, langarch.RVal)
	if !ok {
		return 0, fmt.Errorf("failed to find the %s register", langarch.RVal)
	}
	rn.SetValue(lisp.Int(n))
	if trace {
		rval.Trace = registertest.TraceFunc(m.testing, m.M().Symbols)
	}
	err = t.Start()
	if err != nil {
		return 0, err
	}
	if trace {
		m.testing.Logf("NUMSTEP %d", t.NumStep())
	}
	x, ok := lisp.GetInt(rval.GetValue())
	if !ok {
		return 0, fmt.Errorf("result in not an int: %v", x)
	}
	return x, nil
}

func TestProgram_fact(t *testing.T) {
	fact, err := newFactMachine(t)
	require.NoError(t, err)
	assertFact(t, 1)(fact, 0, true)
	assertFact(t, 1)(fact, 1, true)
	assertFact(t, 2)(fact, 2, true)
	assertFact(t, 6)(fact, 3, true)
	assertFact(t, 24)(fact, 4, true)
	assertFact(t, 120)(fact, 5, true)
	assertFact(t, 3628800)(fact, 10, true)
	assertFact(t, 2432902008176640000)(fact, 20, true)
}

func assertFact(t testing.TB, expect int) func(m *factMachine, n int, trace bool) {
	return func(m *factMachine, n int, trace bool) {
		t.Helper()
		c, err := m.Fact(n, trace)
		if assert.NoError(t, err, "Computation error") {
			assert.Equal(t, expect, c, "Value is incorrect: Fact(%d)", n)
		}
	}
}

func BenchmarkProgram_fact(b *testing.B) {
	fact, err := newFactMachine(b)
	require.NoError(b, err)
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		assertFact(b, 2432902008176640000)(fact, 20, false)
	}
}
