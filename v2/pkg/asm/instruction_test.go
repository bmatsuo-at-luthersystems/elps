package asm

import (
	"strings"
	"testing"

	"github.com/luthersystems/elps/v2/pkg/lisp"
	"github.com/luthersystems/elps/v2/pkg/symbol"
	"github.com/stretchr/testify/assert"
)

var (
	TAssign = symbol.Intern("assign")
	TTest   = symbol.Intern("test")
	TSave   = symbol.Intern("save")
	TGoto   = symbol.Intern("goto")
	TLabel  = symbol.Intern("label")
	TReg    = symbol.Intern("reg")
	TOp     = symbol.Intern("op")
	TConst  = symbol.Intern("const")
)

func TestParseProgram_ok(t *testing.T) {
	table := symbol.NewTable(symbol.DefaultGlobalTable.Export()...)
	abc := sym("abc", table)
	hello := sym("hello", table)
	for i, test := range []struct {
		inst   lisp.LVal
		expect *Instruction
	}{
		{
			lisp.List(nil,
				lisp.Symbol(TSave),
			),
			&Instruction{Type: TSave, Params: nil},
		},
		{
			lisp.List(nil,
				lisp.Symbol(TGoto),
				lisp.List(nil,
					lisp.Symbol(TLabel),
					abc,
				),
			),
			&Instruction{Type: TGoto, Params: []Param{{TLabel, abc}}},
		},
		{
			lisp.List(nil,
				lisp.Symbol(TAssign),
				lisp.List(nil,
					lisp.Symbol(TReg),
					abc,
				),
				lisp.List(nil,
					lisp.Symbol(TOp),
					hello,
				),
			),
			&Instruction{Type: TAssign, Params: []Param{{TReg, abc}, {TOp, hello}}},
		},
	} {
		w := &strings.Builder{}
		_, err := lisp.Format(w, test.inst, table)
		assert.NoError(t, err, "test %d: format error", i)
		t.Logf("test %d: %v", i, w.String())
		inst, err := ParseInstruction(test.inst)
		assert.NoError(t, err, "test %d: error", i)
		assert.Equal(t, test.expect.Type, inst.Type, "test %d: type mismatch", i)
		if assert.Equal(t, len(test.expect.Params), len(inst.Params), "test %d: param list length mismatch", i) {
			for j, p := range inst.Params {
				assert.Equal(t, test.expect.Params[j].Type, p.Type, "test %d: param %d: type mismatch", i, j)
				assert.True(t, lisp.Equal(test.expect.Params[j].Value, p.Value), "test %d: param %d: value mismatch", i, j)
			}
		}
	}
}

func TestParseInstruction_ok(t *testing.T) {
	table := symbol.NewTable(symbol.DefaultGlobalTable.Export()...)
	abc := sym("abc", table)
	hello := sym("hello", table)
	for i, test := range []struct {
		inst   lisp.LVal
		expect *Instruction
	}{
		{
			lisp.List(nil,
				lisp.Symbol(TSave),
			),
			&Instruction{Type: TSave, Params: nil},
		},
		{
			lisp.List(nil,
				lisp.Symbol(TGoto),
				lisp.List(nil,
					lisp.Symbol(TLabel),
					abc,
				),
			),
			&Instruction{Type: TGoto, Params: []Param{{TLabel, abc}}},
		},
		{
			lisp.List(nil,
				lisp.Symbol(TAssign),
				lisp.List(nil,
					lisp.Symbol(TReg),
					abc,
				),
				lisp.List(nil,
					lisp.Symbol(TOp),
					hello,
				),
			),
			&Instruction{Type: TAssign, Params: []Param{{TReg, abc}, {TOp, hello}}},
		},
	} {
		w := &strings.Builder{}
		_, err := lisp.Format(w, test.inst, table)
		assert.NoError(t, err, "test %d: format error", i)
		t.Logf("test %d: %v", i, w.String())
		inst, err := ParseInstruction(test.inst)
		assert.NoError(t, err, "test %d: error", i)
		assert.Equal(t, test.expect.Type, inst.Type, "test %d: type mismatch", i)
		if assert.Equal(t, len(test.expect.Params), len(inst.Params), "test %d: param list length mismatch", i) {
			for j, p := range inst.Params {
				assert.Equal(t, test.expect.Params[j].Type, p.Type, "test %d: param %d: type mismatch", i, j)
				assert.True(t, lisp.Equal(test.expect.Params[j].Value, p.Value), "test %d: param %d: value mismatch", i, j)
			}
		}
	}
}

func TestParseInstruction_error(t *testing.T) {
	table := symbol.NewTable(symbol.DefaultGlobalTable.Export()...)
	abc := sym("abc", table)
	for i, test := range []struct {
		inst lisp.LVal
	}{
		{
			lisp.Nil(),
		},
		{
			lisp.List(nil,
				lisp.String("bad"),
			),
		},
		{
			lisp.List(nil,
				lisp.Symbol(TGoto),
				lisp.List(nil,
					lisp.String("bad"),
					abc,
				),
			),
		},
	} {
		w := &strings.Builder{}
		_, err := lisp.Format(w, test.inst, table)
		if !assert.NoError(t, err, "test %d: format error", i) {
			continue
		}
		t.Logf("test %d: %v", i, w.String())
		_, err = ParseInstruction(test.inst)
		assert.Error(t, err, "test %d: error", i)
	}
}

func TestParseParam_ok(t *testing.T) {
	table := symbol.NewTable(symbol.DefaultGlobalTable.Export()...)
	abc := sym("abc", table)
	hello := sym("hello", table)
	for i, test := range []struct {
		param  lisp.LVal
		expect Param
	}{
		{
			lisp.List(nil, lisp.Symbol(TReg), abc),
			Param{Type: TReg, Value: abc},
		},
		{
			lisp.List(nil, lisp.Symbol(TOp), hello),
			Param{Type: TOp, Value: hello},
		},
		{
			lisp.List(nil, lisp.Symbol(TConst), lisp.List(nil, lisp.Int(1))),
			Param{Type: TConst, Value: lisp.List(nil, lisp.Int(1))},
		},
	} {
		w := &strings.Builder{}
		_, err := lisp.Format(w, test.param, table)
		if !assert.NoError(t, err, "test %d: format error", i) {
			continue
		}
		t.Logf("test %d: %v", i, w.String())
		p, err := ParseParam(test.param)
		if !assert.NoError(t, err, "test %d: error", i) {
			continue
		}
		assert.Equal(t, test.expect.Type, p.Type, "test %d: type mismatch", i)
		assert.True(t, lisp.Equal(test.expect.Value, p.Value), "test %d: value mismatch", i)
	}
}

func TestParseParam_error(t *testing.T) {
	table := symbol.NewTable(symbol.DefaultGlobalTable.Export()...)
	abc := sym("abc", table)
	hello := sym("hello", table)
	for i, test := range []struct {
		param lisp.LVal
	}{
		{
			lisp.Nil(),
		},
		{
			lisp.List(nil, lisp.Symbol(TReg)),
		},
		{
			lisp.List(nil, lisp.Symbol(TOp), hello, abc),
		},
		{
			lisp.List(nil, lisp.Int(3), abc),
		},
	} {
		w := &strings.Builder{}
		_, err := lisp.Format(w, test.param, table)
		if !assert.NoError(t, err, "test %d: format error", i) {
			continue
		}
		t.Logf("test %d: %v", i, w.String())
		_, err = ParseParam(test.param)
		assert.Error(t, err, "test %d: error", i)
	}
}

func TestWriteSymbol(t *testing.T) {
	table := symbol.NewTable(symbol.DefaultGlobalTable.Export()...)
	abc := sym("abc", table)
	hello := sym("hello", table)
	world := sym("world", table)
	for i, test := range []struct {
		expect string
		sym    lisp.LVal
	}{
		{"abc", abc},
		{"hello", hello},
		{"world", world},
	} {
		id, ok := lisp.GetSymbol(test.sym)
		assert.True(t, ok, "test %d: value must be a symbol", i)
		w := &strings.Builder{}
		n, err := writeSymbol(w, id, table)
		assert.NoError(t, err, "test %d: error", i)
		assert.Equal(t, test.expect, w.String(), "test %d: formatted string", i)
		assert.Equal(t, len(test.expect), n, "test %d: format length", i)
	}
}

func param(typ symbol.ID, value lisp.LVal) Param {
	return Param{
		Type:  typ,
		Value: value,
	}
}

func sym(s string, table symbol.Table) lisp.LVal {
	return lisp.Symbol(table.Intern(s))
}

func TestParamFormat(t *testing.T) {
	table := symbol.NewTable(symbol.DefaultGlobalTable.Export()...)
	abc := sym("abc", table)
	hello := sym("hello", table)
	world := sym("world", table)
	for i, test := range []struct {
		expect string
		op     Param
	}{
		{"(label abc)", param(TLabel, abc)},
		{"(reg hello)", param(TReg, hello)},
		{"(op world)", param(TOp, world)},
		{"(const 1)", param(TConst, lisp.Int(1))},
		{"(const ())", param(TConst, lisp.Nil())},
	} {
		w := &strings.Builder{}
		n, err := test.op.Format(w, table)
		assert.NoError(t, err, "test %d: error", i)
		assert.Equal(t, test.expect, w.String(), "test %d: formatted string", i)
		assert.Equal(t, len(test.expect), n, "test %d: format length", i)
	}
}

func instruction(typ symbol.ID, params ...Param) *Instruction {
	return &Instruction{
		Type:   typ,
		Params: params,
	}
}

func TestInstructionFormat(t *testing.T) {
	table := symbol.NewTable(symbol.DefaultGlobalTable.Export()...)
	abc := sym("abc", table)
	hello := sym("hello", table)
	world := sym("world", table)
	for i, test := range []struct {
		expect string
		inst   *Instruction
	}{
		{"(assign (reg abc) (const ()))", instruction(TAssign, param(TReg, abc), param(TConst, lisp.Nil()))},
		{"(save (reg abc))", instruction(TSave, param(TReg, abc))},
		{"(test (op hello) (reg world))", instruction(TTest, param(TOp, hello), param(TReg, world))},
		{"(goto (label world))", instruction(TGoto, param(TLabel, world))},
	} {
		w := &strings.Builder{}
		n, err := test.inst.Format(w, table)
		assert.NoError(t, err, "test %d: error", i)
		assert.Equal(t, test.expect, w.String(), "test %d: formatted string", i)
		assert.Equal(t, len(test.expect), n, "test %d: format length", i)
	}
}
