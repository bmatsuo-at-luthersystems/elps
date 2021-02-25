package asm

import (
	"fmt"
	"io"
	"strings"

	"github.com/luthersystems/elps/v2/pkg/internal/lfmt"
	"github.com/luthersystems/elps/v2/pkg/lisp"
	"github.com/luthersystems/elps/v2/pkg/symbol"
)

// FormatProgram renders p as a human readable assembly program.  Instructions
// and labels are formatted followed by a newline.  Instructions are prefixed
// with the string indent.
func FormatProgram(w io.Writer, p []*Instruction, t symbol.Table, indent string) (int, error) {
	cw := lfmt.NewCountingWriter(w)
	for i, inst := range p {
		for j, label := range inst.Labels {
			_, err := writeSymbol(cw, label, t)
			if err != nil {
				return cw.N(), fmt.Errorf("inst %d: label %d: %w", i, j, err)
			}
			_, err = io.WriteString(cw, "\n")
			if err != nil {
				return cw.N(), fmt.Errorf("inst %d: label %d: newline: %w", i, j, err)
			}
		}
		_, err := io.WriteString(cw, indent)
		if err != nil {
			return cw.N(), fmt.Errorf("inst %d: indent: %w", i, err)
		}
		_, err = cw.DeferCount(func(w io.Writer) (int, error) { return inst.Format(w, t) })
		if err != nil {
			return cw.N(), fmt.Errorf("inst %d: %w", i, err)
		}
		_, err = io.WriteString(cw, "\n")
		if err != nil {
			return cw.N(), fmt.Errorf("inst %d: newline: %w", i, err)
		}
	}
	return cw.N(), nil
}

// InstructionString renders inst as a human readable string, using t to
// resolve symbols.
func InstructionString(inst *Instruction, t symbol.Table) (string, error) {
	w := &strings.Builder{}
	_, err := inst.Format(w, t)
	if err != nil {
		return "", err
	}
	return w.String(), nil
}

// InstructionString renders inst as a human readable string, using t to
// resolve symbols.
func ParamString(p Param, t symbol.Table) (string, error) {
	w := &strings.Builder{}
	_, err := p.Format(w, t)
	if err != nil {
		return "", err
	}
	return w.String(), nil
}

// Instruction is a machine instruction.  An instruction has optional labels.
type Instruction struct {
	Type   symbol.ID
	Labels []symbol.ID
	Params []Param
}

// NewInstruction returns a new instruction with the given type and params.
func NewInstruction(typ symbol.ID, p ...Param) *Instruction {
	return &Instruction{
		Type:   typ,
		Params: p,
	}
}

// ParseProgram creates a slice of instructions based on the lval
// representation of a program.  Along with parsed instructions a slice is
// returned containing any trailing labels which are not attached to an
// instruction and it is up to the assembler whether or not that signals an
// error condition.
func ParseProgram(v lisp.LVal) ([]*Instruction, []symbol.ID, error) {
	var prog []*Instruction
	var labels []symbol.ID // to attach to the next instruction
	for !lisp.IsNil(v) {
		cons, ok := lisp.GetCons(v)
		if !ok {
			return nil, nil, fmt.Errorf("program is not a cons list: %v", v.Type())
		}
		linst := cons.CAR()
		v = cons.CDR()

		label, ok := lisp.GetSymbol(linst)
		if ok {
			labels = append(labels, label)
			continue
		}

		inst, err := ParseInstruction(linst)
		if err != nil {
			return nil, nil, err
		}
		inst.Labels = labels
		prog = append(prog, inst)
	}
	return prog, labels, nil
}

// ParseInstruction creates a instruction based on an LVal representation.
// The LVal should have the following form:
//		(cons Type ParamList)
// Where ParamList has the following form:
//		nil || (cons Param ParamList)
// ParseInstruction does not parse labels.  A higher level function needs to
// parse the whole program and attach labels to instructions.
func ParseInstruction(inst lisp.LVal) (*Instruction, error) {
	cons, ok := lisp.GetCons(inst)
	if !ok {
		return nil, fmt.Errorf("instruction is not a cons list: %v", inst.Type())
	}
	litype, lplist := cons.CAR(), cons.CDR()
	itype, ok := lisp.GetSymbol(litype)
	if !ok {
		return nil, fmt.Errorf("instruction type is not a symbol: %v", litype.Type())
	}
	plist, err := parsePList(lplist)
	if err != nil {
		return nil, err
	}
	_inst := &Instruction{
		Type:   itype,
		Params: plist,
	}
	return _inst, nil
}

func parsePList(v lisp.LVal) ([]Param, error) {
	var plist []Param
	for !lisp.IsNil(v) {
		cons, ok := lisp.GetCons(v)
		if !ok {
			return nil, fmt.Errorf("instruction parameter list is not a list: %v", v.Type())
		}
		lp := cons.CAR()
		v = cons.CDR()
		p, err := ParseParam(lp)
		if err != nil {
			return nil, err
		}
		plist = append(plist, p)
	}
	return plist, nil
}

// Format writes a human readable representation of inst to w.
func (inst *Instruction) Format(w io.Writer, t symbol.Table) (int, error) {
	cw := lfmt.NewCountingWriter(w)
	_, err := io.WriteString(cw, "(")
	if err != nil {
		return cw.N(), err
	}
	_, err = writeSymbol(cw, inst.Type, t)
	if err != nil {
		return cw.N(), err
	}
	for _, p := range inst.Params {
		_, err = io.WriteString(cw, " ")
		if err != nil {
			return cw.N(), err
		}
		_, err = cw.DeferCount(func(w io.Writer) (int, error) { return p.Format(w, t) })
		if err != nil {
			return cw.N(), err
		}
	}
	_, err = io.WriteString(cw, ")")
	return cw.N(), err
}

// Param is an instruction parameter. A parameter contains a primitive LVal
// (non-reference).
type Param struct {
	Type  symbol.ID
	Value lisp.LVal
}

// P returns a Param with the given type and value.
func P(typ symbol.ID, val lisp.LVal) Param {
	return Param{
		Type:  typ,
		Value: val,
	}
}

var p0 Param

// ParseParam creates an instruction parameter based on an LVal representation.
func ParseParam(p lisp.LVal) (Param, error) {
	if lisp.IsNil(p) {
		return p0, fmt.Errorf("instruction paramater is an empty list")
	}
	cons, ok := lisp.GetCons(p)
	if !ok {
		return p0, fmt.Errorf("instruction parameter is not a list: %v", p.Type())
	}
	s, rest, ok := cons.Slice(0, 2)
	if !ok {
		return p0, fmt.Errorf("instruction parameter is not a list: %v", rest.Type())
	}
	if !lisp.IsNil(rest) {
		return p0, fmt.Errorf("instruction parameter has too many list elements")
	}
	if len(s) != 2 {
		return p0, fmt.Errorf("instruction paramater has too few list elements")
	}
	ltyp, val := s[0], s[1]
	typ, ok := lisp.GetSymbol(ltyp)
	if !ok {
		return p0, fmt.Errorf("instruction parameter type is not a symbol: %v", ltyp.Type())
	}
	return Param{Type: typ, Value: val}, nil
}

// Format writes a human readable representation of p to w.
func (p *Param) Format(w io.Writer, t symbol.Table) (int, error) {
	cw := lfmt.NewCountingWriter(w)
	_, err := io.WriteString(cw, "(")
	if err != nil {
		return cw.N(), err
	}
	_, err = writeSymbol(cw, p.Type, t)
	if err != nil {
		return cw.N(), err
	}
	_, err = io.WriteString(cw, " ")
	if err != nil {
		return cw.N(), err
	}
	_, err = cw.DeferCount(func(w io.Writer) (int, error) { return lisp.Format(w, p.Value, t) })
	if err != nil {
		return cw.N(), err
	}
	_, err = io.WriteString(cw, ")")
	return cw.N(), err
}

func writeSymbol(w io.Writer, sym symbol.ID, t symbol.Table) (int, error) {
	s, ok := t.Symbol(sym)
	if !ok {
		return 0, fmt.Errorf("unknown symbol: %x", sym)
	}
	return io.WriteString(w, s)
}
