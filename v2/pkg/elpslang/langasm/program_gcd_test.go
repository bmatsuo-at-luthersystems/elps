package langasm

import (
	"fmt"
	"runtime"
	"sync"
	"sync/atomic"
	"testing"
	"time"

	"github.com/luthersystems/elps/v2/pkg/elpslang/langarch"
	"github.com/luthersystems/elps/v2/pkg/elpslang/langop"
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

var LUninitializedRegister = symbol.Intern("unitialized-register")

func uninit() lisp.LVal { return lisp.TagNative(LUninitializedRegister, nil) }

// The following example is modified from Structure and Interpretation of
// Computer Programs.
var (
	gcdRegA       = symbol.Intern("a")
	gcdRegB       = symbol.Intern("b")
	gcdRegT       = symbol.Intern("t")
	gcdThreadArch = register.MustArch(
		gcdRegA,
		gcdRegB,
		gcdRegT,
		langarch.RFlag,
		langarch.RCondition,
	)
	gcdProgram = `
		test-b
			(test (op =) (reg b) (const 0))
			(branch (label gcd-done))
			(assign (reg t) (op mod) (reg a) (reg b))
			(assign (reg a) (reg b))
			(assign (reg b) (reg t))
			(goto (label test-b))
		gcd-done
	`
)

type gcdMachine struct {
	rmach.Machine
	rmach.Assembler
	testing testing.TB
}

var gcdArch = rmach.MustArch(register.MustArch(), gcdThreadArch)

func newGCDMachine(t testing.TB) (*gcdMachine, error) {
	m, err := rmach.NewMachine(gcdArch, nil, langop.Ops)
	if err != nil {
		return nil, err
	}
	gcd := &gcdMachine{
		Machine:   m,
		Assembler: NewAssembler(gcdArch),
		testing:   t,
	}

	pname := m.M().Symbols.Intern("main")
	program, err := bootstrapparser.ParseProgramString(pname, gcd.M().Symbols, gcdProgram)
	if err != nil {
		return nil, fmt.Errorf("program parse error: %w", err)
	}
	obj, err := gcd.Assemble(program)
	if err != nil {
		return nil, err
	}
	err = obj.Link(gcd, pname)
	if err != nil {
		return nil, err
	}
	return gcd, nil
}

func (m *gcdMachine) GCD(a, b int, trace bool) (int, error) {
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
	ra, ok := register.Lookup(thd, gcdRegA)
	if !ok {
		return 0, fmt.Errorf("failed to find the %s register", gcdRegA)
	}
	rb, ok := register.Lookup(thd, gcdRegB)
	if !ok {
		return 0, fmt.Errorf("failed to find the %s register", gcdRegB)
	}
	ra.SetValue(lisp.Int(a))
	if trace {
		ra.Trace = registertest.TraceFunc(m.testing, m.M().Symbols)
	}
	rb.SetValue(lisp.Int(b))
	err = t.Start()
	if err != nil {
		return 0, err
	}
	if trace {
		m.testing.Logf("NUMSTEP %d", t.NumStep())
	}
	x, ok := lisp.GetInt(ra.GetValue())
	if !ok {
		return 0, fmt.Errorf("result in not an int: %v", x)
	}
	return x, nil
}

func TestProgram_gcd(t *testing.T) {
	gcd, err := newGCDMachine(t)
	require.NoError(t, err)
	assertGCD(t, 64)(gcd, 0, 64, true)
	assertGCD(t, 64)(gcd, 64, 0, true)
	assertGCD(t, 2)(gcd, 2, 4, true)
	assertGCD(t, 2)(gcd, 4, 2, true)
	assertGCD(t, 3)(gcd, 6, 9, true)
	assertGCD(t, 3)(gcd, 9, 6, true)
	assertGCD(t, 8)(gcd, 64, 24, true)
	assertGCD(t, 8)(gcd, 24, 64, true)
	assertGCD(t, 1)(gcd, 5003, 7919, true)
}

func TestProgram_gcd_parallel(t *testing.T) {
	gcd, err := newGCDMachine(t)
	require.NoError(t, err)
	var wg sync.WaitGroup
	var numgcd int64
	start := make(chan struct{})
	done := make(chan struct{})
	n := runtime.NumCPU()
	wg.Add(n)
	for i := 0; i < n; i++ {
		go func(i int) {
			<-start
			for {
				for j := 0; j < 10; j++ {
					assertGCD(t, 1)(gcd, 5003, 7919, false)
					atomic.AddInt64(&numgcd, 1)
				}
				select {
				case <-done:
					wg.Done()
					return
				default:
				}
			}
		}(i)
	}
	go func() {
		close(start)
		<-time.After(250 * time.Millisecond)
		close(done)
	}()
	wg.Wait()
	t.Logf("numgcd: %d", numgcd)
}

func assertGCD(t testing.TB, expect int) func(m *gcdMachine, a, b int, trace bool) {
	return func(m *gcdMachine, a, b int, trace bool) {
		t.Helper()
		c, err := m.GCD(a, b, trace)
		if assert.NoError(t, err, "Computation error") {
			assert.Equal(t, expect, c, "Value is incorrect: GCD(%d, %d)", a, b)
		}
	}
}

func BenchmarkProgram_gcd(b *testing.B) {
	gcd, err := newGCDMachine(b)
	require.NoError(b, err)
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		assertGCD(b, 1)(gcd, 5003, 7919, false)
	}
}
