package symbol

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestString(t *testing.T) {
	table := NewTable()
	hello := table.Intern("hello")
	assert.Equal(t, "hello", String(hello, table))
	assert.Equal(t, "#<SYMBOL 0x123456789abcdef0>", String(0x123456789abcdef0, table))
}
