package stack

/*

func assertLError(t *testing.T, v lisp.LVal, extra ...interface{}) bool {
	t.Helper()
	return assert.Equal(t, lisp.LError, v.Type(), extra...)
}

func assertNoLError(t *testing.T, v lisp.LVal, extra ...interface{}) bool {
	t.Helper()
	return assert.NotEqual(t, lisp.LError, v.Type(), extra...)
}

func TestGetStack(t *testing.T) {
	s := New()
	_s, ok := GetStack(s.LVal(nil, 0))
	if assert.True(t, ok) {
		assert.Equal(t, s, _s)
	}
	_, ok = GetStack(lisp.Int(nil, 1))
	assert.False(t, ok)
	_, ok = GetStack(lisp.Nil(nil))
	assert.False(t, ok)
}

func TestStack(t *testing.T) {
	s := New().LVal(nil, 0)
	assertNoLError(t, Push(s, lisp.Int(nil, 5)))
	v := Pop(s)
	if assertNoLError(t, v) {
		x, ok := lisp.GetInt(v)
		_ = assert.True(t, ok) &&
			assert.Equal(t, 5, x)
	}
	assertNoLError(t, Initialize(s))
	assertLError(t, Pop(s))
	assertNoLError(t, Push(s, lisp.Int(nil, 1)))
	assertNoLError(t, Push(s, lisp.Int(nil, 2)))
	v = Pop(s)
	if assertNoLError(t, v) {
		x, ok := lisp.GetInt(v)
		_ = assert.True(t, ok) &&
			assert.Equal(t, 2, x)
	}
	stack, _ := GetStack(s)
	assert.Equal(t, 2, stack.MaxDepth)
	assert.Equal(t, 3, stack.NumPush)
	assert.Equal(t, 1, len(stack.Values))
}
*/
