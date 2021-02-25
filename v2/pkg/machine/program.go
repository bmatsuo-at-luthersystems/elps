package machine

import (
	"fmt"

	"github.com/luthersystems/elps/v2/pkg/asm"
	"github.com/luthersystems/elps/v2/pkg/lisp"
	"github.com/luthersystems/elps/v2/pkg/symbol"
)

// PC is an index into an object program.
type PC struct {
	m      *M
	p      *Program
	offset int
}

var eof = PC{}

// String renders pc as a human readable string
func (pc PC) String() string {
	var name string
	if pc.p != nil && pc.m != nil {
		name = pc.m.symbolString(pc.p.name)
	}
	return fmt.Sprintf("%s:%d", name, pc.offset)
}

// IsEOF returns true if the PC was advanced passed the last program
// instruction.
func (pc PC) IsEOF() bool {
	return pc.p == nil || pc.offset >= len(pc.p.code)
}

// Advance returns a PC for the next instruction in the program's code.
func (pc PC) Advance() PC {
	pc.offset += 1
	return pc
}

// Offset returns PC offset by n instructions
func (pc PC) Offset(n int) PC {
	return PC{
		m:      pc.m,
		p:      pc.p,
		offset: pc.offset + n,
	}
}

// Code returns the code referenced by pc.  If pc is EOF or invalid then false
// is returned.
func (pc PC) Code() (Code, bool) {
	c, ok := pc.code()
	if ok {
		return c.Code, true
	}
	return Code{}, false
}

func (pc PC) code() (*codeInternal, bool) {
	if pc.p == nil || pc.offset < 0 || pc.offset >= len(pc.p.code) {
		return nil, false
	}
	return pc.p.code[pc.offset], true
}

// Labels retuns labels associated with pc.  Labels will only return a value
// for PC after the Program that pc indexes is Installed into its machine.
func (pc PC) Labels() []symbol.ID {
	if pc.p == nil {
		return nil
	}
	return pc.p.rlabels[pc]
}

// LVal wraps pc as an LVal.
func (pc PC) LVal(source lisp.Source, typename symbol.ID) lisp.LVal {
	return lisp.TagNative(typename, &pc)
}

// GetPC extracts a PC from v.  GetPC returns false if v is not LTaggedVal or
// does not contain a PC.
func GetPC(v lisp.LVal) (PC, bool) {
	if v.Type() != lisp.LTaggedVal {
		return eof, false
	}
	pc, ok := v.Native.(*PC)
	if !ok {
		return eof, false
	}
	return *pc, true
}

// Program is an an object program, an assembled unit of code.
type Program struct {
	m       *M
	name    symbol.ID
	labels  map[symbol.ID]PC
	rlabels map[PC][]symbol.ID
	code    []*codeInternal
}

func newProgram(m *M, name symbol.ID) *Program {
	return &Program{
		m:       m,
		name:    name,
		labels:  make(map[symbol.ID]PC),
		rlabels: make(map[PC][]symbol.ID),
	}
}

// Start returns a PC for the first instruction instruction in p.
func (p *Program) Start() PC {
	return p.Offset(0)
}

// EOF returns a PC that is just beyond the end of p.Code.
func (p *Program) EOF() PC {
	return p.Offset(len(p.code))
}

// Offset returns a PC that points to p.Code[i].  The referenced instruction
// does not need to exist.
func (p *Program) Offset(i int) PC {
	return PC{
		m:      p.m,
		p:      p,
		offset: i,
	}
}

// DefineLabel defines a label in the program at the instruction index offset
// and returns the corresponding PC.  The label defined with DefineLabel can
// have any non-negative offset, regardless of p's length.  DefineLabel returns
// an error if label is already defined in the program or if the offset is
// negative.  DefineLabel does not check if label is already defined in the
// machine.
func (p *Program) DefineLabel(label symbol.ID, offset int) (PC, error) {
	_, ok := p.labels[label]
	if ok {
		return eof, fmt.Errorf("duplicate label defined in program %s: %v", p.name, p.m.symbolString(label))
	}
	if offset < 0 {
		return eof, fmt.Errorf("label given negative offset: %d", offset)
	}
	pc := p.Offset(offset)
	p.labels[label] = pc
	p.rlabels[pc] = append(p.rlabels[pc], label)
	return pc, nil
}

// Append creates a new code and appends it to the end of p.
func (p *Program) Append(text *asm.Instruction, exec Executor) {
	p.appendInternal(&codeInternal{
		Code: Code{
			Text: text,
			Exec: exec,
		},
	})
}

// AppendCode adds inst to the program as the last instruction and
// applies any labels.  AppendCode panics if an instruction pointer is
// nil.
func (p *Program) AppendCode(code ...Code) {
	for i := range code {
		p.appendInternal(&codeInternal{Code: code[i]})
	}
}

func (p *Program) appendInternal(code *codeInternal) {
	p.code = append(p.code, code)
}
