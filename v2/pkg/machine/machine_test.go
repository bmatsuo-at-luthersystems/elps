package machine

import (
	"testing"
	"time"

	"github.com/luthersystems/elps/v2/pkg/asm"
	"github.com/luthersystems/elps/v2/pkg/symbol"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func testM(t *testing.T) *M {
	m := New()
	m.Symbols = symbol.NewTable(symbol.DefaultGlobalTable.Export()...)
	return m
}

// Test basic machine functionality that can be relied on in future tests.
func TestMachine_NopExec(t *testing.T) {
	m := testM(t)
	_, err := m.EntryPoint()
	require.Error(t, err)
	_, err = m.NewThread()
	require.Error(t, err)
	p0 := m.NewProgram(m.Symbols.Intern("p0"))
	err = m.Install(p0)
	require.NoError(t, err)
	pc, err := m.EntryPoint()
	if assert.NoError(t, err) {
		assert.Equal(t, PC{m: m, p: p0, offset: 0}, pc)
	}
	thd, err := m.NewThread()
	if assert.NoError(t, err) {
		assert.NotNil(t, thd)
		err := thd.Start()
		assert.NoError(t, err)
		err = thd.Stop(false)
		assert.NoError(t, err)
		assert.Nil(t, thd.Data)
	}
}

func TestMachine_SyncStopWait(t *testing.T) {
	m := testM(t)
	p0 := m.NewProgram(m.Symbols.Intern("p0"))
	err := m.Install(p0)
	require.NoError(t, err)
	thd, err := m.NewThread()
	if assert.NoError(t, err) {
		err := thd.Start()
		assert.NoError(t, err)
		err = thd.Stop(true)
		assert.NoError(t, err)
		assert.Nil(t, thd.Data)
	}
}

func TestMachine_SimpleExec(t *testing.T) {
	m := testM(t)
	p0 := m.NewProgram(m.Symbols.Intern("p0"))
	p0.Append(asm.NewInstruction(m.Symbols.Intern("i0")), func(thd *T) error {
		thd.Data = 1
		thd.AdvancePC()
		return nil
	})
	err := m.Install(p0)
	require.NoError(t, err)
	pc, err := m.EntryPoint()
	if assert.NoError(t, err) {
		assert.Equal(t, PC{m: m, p: p0, offset: 0}, pc)
	}
	thd, err := m.NewThread()
	if assert.NoError(t, err) {
		err := thd.Start()
		assert.NoError(t, err)
		err = thd.Stop(false)
		assert.NoError(t, err)
		assert.Equal(t, 1, thd.Data)
	}
}

func TestMachine_ConcurrentStopWait(t *testing.T) {
	m := testM(t)
	p0 := m.NewProgram(m.Symbols.Intern("p0"))
	p0.Append(asm.NewInstruction(m.Symbols.Intern("i0")), func(thd *T) error {
		time.Sleep(250 * time.Millisecond)
		thd.AdvancePC()
		return nil
	})
	err := m.Install(p0)
	require.NoError(t, err)
	thd, err := m.NewThread()
	require.NoError(t, err)
	timeout := time.NewTimer(500 * time.Millisecond)
	defer timeout.Stop()
	doneExec := make(chan struct{})
	doneWait := make(chan struct{})

	go func() {
		err := thd.Start()
		assert.NoError(t, err)
		close(doneExec)
	}()

	time.Sleep(5 * time.Millisecond)
	err = thd.Stop(false)
	assert.Error(t, err)

	go func() {
		err = thd.Stop(true)
		assert.NoError(t, err)
		close(doneWait)
	}()

	select {
	case <-timeout.C:
		t.Fatalf("timed out waiting for exec")
	case <-doneExec:
	}
	select {
	case <-timeout.C:
		t.Fatalf("timed out waiting for stop")
	case <-doneWait:
	}
}
