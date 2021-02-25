package lisp

import (
	"testing"

	"github.com/luthersystems/elps/v2/pkg/symbol"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

/*
func testConstructorSource(t *testing.T, fn func(s Source) LVal) {
	t.Helper()
	loc := floc("main.lisp", 10)
	v := fn(loc)
	assert.Equal(t, loc.File, v.SourceFile())
	assert.Equal(t, loc.Line, v.SourceLine())
}
*/

func TestNil(t *testing.T) {
	var _nil LVal
	require.Equal(t, LNil, _nil.Type())
	v := Nil()
	require.Equal(t, LNil, v.Type())
}

func TestString(t *testing.T) {
	for _, x := range []string{
		"", "hello", "t",
	} {
		v := String(x)
		require.NotEqual(t, LError, v.Type(), "input: %v", x)
		y, ok := GetString(v)
		assert.True(t, ok, "input: %v", x)
		assert.Equal(t, x, y, "input: %v", x)
	}
}

func TestSymbol(t *testing.T) {
	for _, x := range []symbol.ID{
		0, 1, 1000,
	} {
		v := Symbol(x)
		require.NotEqual(t, LError, v.Type(), "input: %v", x)
		y, ok := GetSymbol(v)
		assert.True(t, ok, "input: %v", x)
		assert.Equal(t, x, y, "input: %v", x)
	}
}

func TestInt(t *testing.T) {
	for _, x := range []int{
		0, 1, -1, 256, -255, 100000,
	} {
		v := Int(x)
		require.NotEqual(t, LError, v.Type(), "input: %v", x)
		y, ok := GetInt(v)
		assert.True(t, ok, "input: %v", x)
		assert.Equal(t, x, y, "input: %v", x)
	}
}

func floc(file string, line int) *simpleSource {
	return &simpleSource{
		File: file,
		Line: line,
	}
}

// simpleSource is both a SourceLocation and a Source for simplicity
type simpleSource struct {
	File string
	Line int
}

var _ SourceLocation = (*simpleSource)(nil)

func (loc *simpleSource) SourceFile() string             { return loc.File }
func (loc *simpleSource) SourceLine() int                { return loc.Line }
func (loc *simpleSource) SourceLocation() SourceLocation { return loc }
