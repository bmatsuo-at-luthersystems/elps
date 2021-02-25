/*
Package machine provides a register based virtual machine that uses lisp.LVal
as the primitive data type which is stored in registers on manipulated by
operators.
*/
package machine

import (
	"fmt"
	"sync"
	"sync/atomic"

	"github.com/luthersystems/elps/v2/pkg/symbol"
)

// M is a register-based virtual machine.  Registers are identified by symbols
// and are manipulated with machine instructions and operations.
type M struct {
	// Symbols contains an optional symbol table for the machine.  An M only
	// consults Symbols to render diagnostic strings for error messages.  If
	// Symbols is nil then symbol.DefaultGlobalTable will be used to render
	// diagnostic strings.
	Symbols symbol.Table

	// Objects holds object programs which may be entered during execution.
	// Programs are never removed from objects.
	objects map[symbol.ID]*Program
	entry   symbol.ID

	tid     int64
	threads map[int]*T
	tmut    sync.RWMutex
}

// New returns a new machine.  The returned machine will have no registers,
// operations, or symbol table pre-installed.  New returns any error
// encountered during installation of the assembler.
func New() *M {
	m := &M{
		objects: make(map[symbol.ID]*Program),
		threads: make(map[int]*T),
	}
	return m
}

// EntryPoint returns a PC for the first instruction of the first installed
// Program.
func (m *M) EntryPoint() (PC, error) {
	m.tmut.RLock()
	defer m.tmut.RUnlock()
	return m.entryPoint()
}

func (m *M) entryPoint() (PC, error) {
	if len(m.objects) == 0 {
		return eof, fmt.Errorf("no program")
	}
	return m.objects[m.entry].Start(), nil
}

func (m *M) Program(name symbol.ID) *Program {
	m.tmut.RLock()
	defer m.tmut.RUnlock()
	return m.objects[name]
}

// NewProgram returns an new program with the given name.  The name is used
// solely for diagnostics and is not required to be unique.
func (m *M) NewProgram(name symbol.ID) *Program {
	return newProgram(m, name)
}

// Install hooks an object program p into m.  Any labels defined in p will be
// used to setup hooks for managing breakpoints.  After calling Install p must
// not be modified.
func (m *M) Install(p *Program) error {
	if p.m == nil || p.m != m {
		return fmt.Errorf("program belongs to a diffeent machine")
	}
	m.tmut.Lock()
	defer m.tmut.Unlock()
	if len(m.objects) == 0 {
		m.entry = p.name
	}
	_, ok := m.objects[p.name]
	if ok {
		return fmt.Errorf("duplicate program name defined: %s", p.name)
	}
	m.objects[p.name] = p
	return nil
}

// LocatePC returns the PC corresponding to specified label in the given
// program.
func (m *M) LocatePC(pname symbol.ID, label symbol.ID) (PC, error) {
	return m.locatePC(pname, label)
}

// LocateCode returns the instruction at an offset from a label in the
// program pname.
func (m *M) LocateCode(pname symbol.ID, label symbol.ID, offset int) (Code, error) {
	c, err := m.locateCode(pname, label, offset)
	if err != nil {
		return Code{}, err
	}
	return c.Code, nil
}

func (m *M) locateCode(pname symbol.ID, label symbol.ID, offset int) (*codeInternal, error) {
	pc, err := m.locatePC(pname, label)
	if err != nil {
		return nil, err
	}
	pc = pc.Offset(offset)
	inst, ok := pc.code()
	if !ok {
		return nil, fmt.Errorf("invalid instruction reference: %v", pc)
	}
	return inst, nil
}

func (m *M) locatePC(pname symbol.ID, label symbol.ID) (PC, error) {
	m.tmut.RLock()
	p, ok := m.objects[pname]
	m.tmut.RUnlock()
	if !ok {
		return eof, fmt.Errorf("unknown program: %v", m.symbolString(label))
	}
	pc, ok := p.labels[label]
	if !ok {
		return eof, fmt.Errorf("label is undefined: %s", m.symbolString(label))
	}
	return pc, nil
}

// SetBreakpoint marks the specified instruction with a breakpoint.
// SetBreakpoint has no effect if called multiple times with the same label and
// offset has no effect.  SetBreakpoint returns an error if the label and
// offset do not form a valid instruction reference.
func (m *M) SetBreakpoint(pname symbol.ID, label symbol.ID, offset int) error {
	code, err := m.locateCode(pname, label, offset)
	if err != nil {
		return err
	}
	code.setBreakpoint(Breakpoint{
		Program: pname,
		Label:   label,
		Offset:  offset,
	})
	return nil
}

// CancelBreakpoint removes the specified breakpoint.  CancelBreakpoint returns
// an error if the specified breakpoint was not previously set or if the label
// and offset do not form a valid instruction reference.
func (m *M) CancelBreakpoint(pname symbol.ID, label symbol.ID, offset int) error {
	code, err := m.locateCode(pname, label, offset)
	if err != nil {
		return err
	}
	bp := Breakpoint{
		Program: pname,
		Label:   label,
		Offset:  offset,
	}
	ok := code.cancelBreakpoint(bp)
	if !ok {
		return fmt.Errorf("breakpoint not set: %v", bp)
	}
	return nil
}

// CancelAllBreakpoints removes all breakpoints from the machine program.
func (m *M) CancelAllBreakpoints() {
	m.tmut.RLock()
	defer m.tmut.RUnlock()

	for _, p := range m.objects {
		for _, inst := range p.code {
			inst.cancelAllBreakpoints()
		}
	}
}

// Kill sends a kill signal to all threads.
func (m *M) Kill() {
	for _, t := range m.threads {
		t.Kill()
	}
}

// NewThread creates a new machine thread.
func (m *M) NewThread() (*T, error) {
	m.tmut.Lock()
	defer m.tmut.Unlock()

	pc, err := m.entryPoint()
	if err != nil {
		return nil, err
	}
	t := &T{
		id:   m.newTID(),
		pc:   pc,
		kill: new(signal),
		pause: sync.Cond{
			L: new(sync.Mutex),
		},
	}
	m.threads[t.id] = t
	return t, nil
}

// rmThread deletes a thread from m
func (m *M) rmThread(t *T) error {
	if t.pc.m != m {
		return fmt.Errorf("thread belongs to a different machine")
	}
	if t.State() != TStopped {
		return fmt.Errorf("thread has not been stopped")
	}

	m.tmut.Lock()
	defer m.tmut.Unlock()
	delete(m.threads, t.id)
	return nil
}

func (m *M) newTID() int {
	return int(atomic.AddInt64(&m.tid, 1))
}

func (m *M) symbols() symbol.Table {
	if m.Symbols != nil {
		return m.Symbols
	}
	return symbol.DefaultGlobalTable
}

func (m *M) symbolString(id symbol.ID) string {
	if m.Symbols != nil {
		return symbol.String(id, m.Symbols)
	}
	return id.String()
}

// signal is a cheap one-way signaling mechanism
type signal int64

func (s *signal) Send() {
	atomic.StoreInt64((*int64)(s), 1)
}

// Handle resets the signal so it may be sent again.  Handle returns true if
// there was an unhandled Send.
func (s *signal) Handle() (recv bool) {
	return atomic.CompareAndSwapInt64((*int64)(s), 1, 0)
}

func (s *signal) Received() bool {
	return atomic.LoadInt64((*int64)(s)) != 0
}

type kill struct{ signal }

func (s *kill) Handle() bool {
	return s.Received()
}
