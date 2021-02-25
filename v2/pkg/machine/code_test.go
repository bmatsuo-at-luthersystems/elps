package machine

import (
	"strings"
	"testing"

	"github.com/luthersystems/elps/v2/pkg/symbol"
	"github.com/stretchr/testify/assert"
)

func TestBreakpoint(t *testing.T) {
	symbols := symbol.NewTable()
	bp := Breakpoint{
		Program: symbols.Intern("p"),
		Label:   symbols.Intern("l"),
		Offset:  5,
	}
	buf := &strings.Builder{}
	_, err := bp.Format(buf, symbols)
	assert.NoError(t, err)
	assert.Equal(t, "p:l[+5]", buf.String())

	bp2 := Breakpoint{
		Program: symbols.Intern("p"),
		Label:   symbols.Intern("m"),
		Offset:  -1,
	}
	buf = &strings.Builder{}
	_, err = bp2.Format(buf, symbols)
	assert.NoError(t, err)
	assert.Equal(t, "p:m[-1]", buf.String())

	code := &codeInternal{}
	assert.False(t, code.isBreakpointSet())

	assert.True(t, code.setBreakpoint(bp))
	assert.True(t, code.isBreakpointSet())

	assert.False(t, code.setBreakpoint(bp))
	assert.True(t, code.isBreakpointSet())

	assert.True(t, code.setBreakpoint(bp2))
	assert.True(t, code.cancelBreakpoint(bp2))
	assert.True(t, code.isBreakpointSet()) // bp still set

	assert.True(t, code.cancelBreakpoint(bp))
	assert.False(t, code.isBreakpointSet())

	assert.True(t, code.setBreakpoint(bp2))
	assert.True(t, code.isBreakpointSet())

	code.cancelAllBreakpoints()
	assert.False(t, code.isBreakpointSet())
}

func TestThreadBreakpoint(t *testing.T) {
	m := testM(t)
	pname := m.Symbols.Intern("main")
	p := m.NewProgram(pname)
	execInc := func(thd *T) error {
		thd.Data = thd.Data.(int) + 1
		thd.AdvancePC()
		return nil
	}
	p.Append(nil, execInc)
	p.Append(nil, execInc)
	p.Append(nil, execInc)
	label := m.Symbols.Intern("entry")
	_, err := p.DefineLabel(label, 1)
	assert.NoError(t, err)
	assert.NoError(t, m.Install(p))
	assert.NoError(t, m.SetBreakpoint(pname, label, 1))
	thd, err := m.NewThread()
	assert.NoError(t, err)
	thd.Data = 0
	err = thd.Start()
	if assert.Error(t, err) {
		_, ok := err.(*BreakpointError)
		assert.True(t, ok)
	}
	assert.Equal(t, 2, thd.Data.(int))
	assert.NoError(t, thd.Stop(false))
}
