package symbol

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestNewIDGen(t *testing.T) {
	g := NewIDGen(0)
	assert.Equal(t, ID(1), g.NewID())
	assert.Equal(t, ID(2), g.NewID())
	assert.Equal(t, ID(3), g.NewID())
}
