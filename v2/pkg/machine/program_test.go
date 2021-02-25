package machine

import (
	"fmt"
	"sort"
	"testing"

	"github.com/luthersystems/elps/v2/pkg/asm"
	"github.com/luthersystems/elps/v2/pkg/symbol"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func nopExec(thd *T) error {
	thd.AdvancePC()
	return nil
}

func sortSymbols(s []symbol.ID) {
	sort.Slice(s, func(i, j int) bool { return s[i] < s[j] })
}

func TestProgram(t *testing.T) {
	m := testM(t)
	p := m.NewProgram(m.Symbols.Intern("p0"))
	pcStart := p.Start()
	assert.Equal(t, m, pcStart.m)
	assert.Equal(t, p, pcStart.p)
	assert.Equal(t, 0, pcStart.offset)
	assert.True(t, pcStart.IsEOF())
	pcEOF := p.EOF()
	assert.Equal(t, m, pcEOF.m)
	assert.Equal(t, p, pcEOF.p)
	assert.Equal(t, pcEOF, pcStart)
	assert.True(t, pcEOF.IsEOF())
	pcOffset := p.Offset(7)
	assert.Equal(t, m, pcOffset.m)
	assert.NotNil(t, pcOffset.m)
	assert.Equal(t, p, pcOffset.p)
	assert.Equal(t, 7, pcOffset.offset)
	assert.True(t, pcOffset.IsEOF())
	var itypes []symbol.ID
	for i := 0; i < 10; i++ {
		it := m.Symbols.Intern(fmt.Sprintf("nop%d", i))
		itypes = append(itypes, it)
		p.Append(asm.NewInstruction(it), nopExec)
	}
	assert.False(t, pcStart.IsEOF())
	inst, ok := pcStart.Code()
	if assert.True(t, ok) {
		assert.Equal(t, itypes[0], inst.Text.Type)
	}
	assert.False(t, pcOffset.IsEOF())
	inst, ok = pcOffset.Code()
	if assert.True(t, ok) {
		assert.Equal(t, itypes[pcOffset.offset], inst.Text.Type)
	}
	assert.True(t, p.EOF().IsEOF())
	inst, ok = p.EOF().Code()
	assert.False(t, ok)
	assert.Nil(t, inst.Text)
	assert.Nil(t, inst.Exec)
}

func TestLabels(t *testing.T) {
	m := testM(t)
	p0 := m.NewProgram(m.Symbols.Intern("p0"))
	err := m.Install(p0)
	require.NoError(t, err)
	assert.Len(t, p0.labels, 0)
	p1 := m.NewProgram(m.Symbols.Intern("p1"))
	lhello := m.Symbols.Intern("hello")
	lentry := m.Symbols.Intern("entry")
	linner := m.Symbols.Intern("inner")
	lgoodbye := m.Symbols.Intern("goodbye")
	lciao := m.Symbols.Intern("ciao")
	lsayonara := m.Symbols.Intern("sayonara")
	pcs := make(map[symbol.ID]PC)
	deflabel := func(label symbol.ID, offset int) {
		t.Helper()

		pcs[label], err = p1.DefineLabel(label, offset)
		if assert.NoError(t, err) {
			assert.Equal(t, p1.Offset(offset), pcs[label])
		}
	}
	deflabel(lhello, 0)
	deflabel(lentry, 0)
	deflabel(linner, 2)
	deflabel(lgoodbye, 3)
	deflabel(lciao, 4)
	deflabel(lsayonara, 5) // way off the end of instructions
	for label, pc := range pcs {
		assert.Equal(t, pc, p1.labels[label], label)
	}
	p1.AppendCode([]Code{
		{
			Text: asm.NewInstruction(m.Symbols.Intern("nop0")),
			Exec: nopExec,
		},
		{
			Text: asm.NewInstruction(m.Symbols.Intern("nop1")),
			Exec: nopExec,
		},
		{
			Text: asm.NewInstruction(m.Symbols.Intern("nop2")),
			Exec: nopExec,
		},
	}...)
	assert.False(t, pcs[lhello].IsEOF())
	assert.False(t, pcs[lentry].IsEOF())
	assert.False(t, pcs[linner].IsEOF())
	assert.True(t, pcs[lgoodbye].IsEOF())
	assert.True(t, pcs[lciao].IsEOF())
	assert.True(t, pcs[lsayonara].IsEOF())
	err = m.Install(p1)
	assert.NoError(t, err)
	allLabels := []symbol.ID{lhello, lentry, linner, lgoodbye, lciao, lsayonara}
	sortSymbols(allLabels)
	assert.Len(t, p1.labels, len(allLabels))
	var p1Labels []symbol.ID
	for label := range p1.labels {
		p1Labels = append(p1Labels, label)
	}
	sortSymbols(p1Labels)
	assert.Equal(t, allLabels, p1Labels)
}

func TestDuplicateLabels(t *testing.T) {
	m := testM(t)
	x := m.Symbols.Intern("x")
	pbad := m.NewProgram(m.Symbols.Intern("pbad"))
	_, err := pbad.DefineLabel(x, 0)
	assert.NoError(t, err)
	_, err = pbad.DefineLabel(x, 0) // duplicate within one program
	assert.Error(t, err)
	p0 := m.NewProgram(m.Symbols.Intern("p0"))
	_, err = p0.DefineLabel(x, 0) // ok
	assert.NoError(t, err)
	assert.NoError(t, m.Install(p0))
	p1 := m.NewProgram(m.Symbols.Intern("p1"))
	_, err = p1.DefineLabel(x, 0) // ok
	assert.NoError(t, err)
	assert.NoError(t, m.Install(p1)) // duplicate across programs are ok
}
