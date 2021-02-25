package symbol

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestTable_Global(t *testing.T) {
	table := newTable()
	assert.Equal(t, ID(1), table.Intern("testing"))
	assert.Equal(t, ID(2), table.Intern("hello"))
	assert.Equal(t, ID(1), table.Intern("testing"))
	id, ok := table.Peek("hello")
	assert.True(t, ok)
	assert.Equal(t, ID(2), id)
	_, ok = table.Peek("notfound")
	assert.False(t, ok)
	s, ok := table.Symbol(1)
	assert.True(t, ok)
	assert.Equal(t, "testing", s)
}

func TestTable_Package(t *testing.T) {
	table := newTable()
	pid := table.Intern("mypkg")
	id := table.Intern("mysymbol")
	p := table.Package(pid)

	_, ok := p.PeekID(pid)
	assert.False(t, ok)

	qid := p.InternID(id)
	assert.NotEqual(t, id, qid)
	s, ok := table.Symbol(qid)
	assert.True(t, ok)
	assert.Equal(t, "mypkg:mysymbol", s)

	_qid, ok := p.PeekID(id)
	assert.True(t, ok)
	assert.Equal(t, qid, _qid)
}
