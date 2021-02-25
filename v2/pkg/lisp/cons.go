package lisp

import (
	"fmt"
	"io"

	"github.com/luthersystems/elps/v2/pkg/internal/lfmt"
	"github.com/luthersystems/elps/v2/pkg/symbol"
)

// ConsData is the container that backs LCons values.
type ConsData struct {
	CAR LVal
	CDR LVal
}

// CloneData implements the DataCloner interface.
func (data *ConsData) CloneData() (DataCloner, error) {
	var err error
	cp := &ConsData{}
	cp.CAR, err = data.CAR.Clone()
	if err != nil {
		return nil, err
	}
	cp.CDR, err = data.CDR.Clone()
	if err != nil {
		return nil, err
	}
	return cp, nil
}

func makeCons(data *ConsData) LVal {
	return LVal{
		LTypeData: Type(LCons),
		Native:    data,
	}
}

// Cons returns a new LCons value from head and tail.  If tail is a list then
// Cons returns a list as well.
// 	(cons head tail)
func Cons(head, tail LVal) LVal {
	return makeCons(&ConsData{
		CAR: head,
		CDR: tail,
	})
}

func GetCAR(v LVal) (LVal, bool) {
	if v.Type() != LCons {
		return Nil(), false
	}
	return v.Native.(*ConsData).CAR, true
}

func GetCDR(v LVal) (LVal, bool) {
	if v.Type() != LCons {
		return Nil(), false
	}
	return v.Native.(*ConsData).CDR, true
}

// Expr returns an unquoted list.
func Expr(v ...LVal) LVal {
	lis := Nil()
	for i := len(v) - 1; i >= 0; i-- {
		lis = Cons(v[i], lis)
	}
	return lis
}

// List returns a quoted list.
func List(s Source, v ...LVal) LVal {
	return Quote(Expr(v...))
}

// ListCompact returns an unquoted list containing the elements of v.  Pointers
// in the returned linked list are guaranteed be short hops but the list data
// cannot be garbance collected until all references to the list and its cdrs
// have been released.
func ExprCompact(v ...LVal) LVal {
	if len(v) == 0 {
		return Nil()
	}

	cons := make([]ConsData, len(v))
	lis := Nil()
	for i := len(v) - 1; i >= 0; i-- {
		cons[i].CAR = v[i]
		cons[i].CDR = lis
		lis = makeCons(&cons[i])
	}
	return lis
}

// ListCompact returns an quoted list containing the elements of v.  See
// ExprCompact for more information.
func ListCompact(v ...LVal) LVal {
	return Quote(ExprCompact(v...))
}

// ConsVal wraps LCons values and provides convenience methods.
type ConsVal struct {
	v    LVal
	data *ConsData
}

func consVal(v LVal) ConsVal {
	c := ConsVal{v: v}
	c.data = c.ConsData()
	return c
}

// NewConsVal constructs ConsVal directly instead of needing to unwrap an LCons
// value.
func NewConsVal(head LVal, tail LVal) *ConsVal {
	data := &ConsData{head, tail}
	return &ConsVal{
		v:    makeCons(data),
		data: data,
	}
}

// GetConsData returns ConsData from v.
// GetConsData returns false if v is not LCons.
func GetConsData(v LVal) (*ConsData, bool) {
	if v.Type() != LCons {
		return nil, false
	}
	return v.Native.(*ConsData), true
}

// GetCons returns a ConsVal for v.
// GetCons returns false if v is not LCons.
func GetCons(v LVal) (ConsVal, bool) {
	if v.Type() != LCons {
		return ConsVal{}, false
	}
	return consVal(v), true
}

// MustCons wraps v as a ConsVal.
// MustCons panics if v.Type() is not LCons.
func MustCons(v LVal) ConsVal {
	if v.Type() != LCons {
		panicf("not a cons: %v", v.Type())
	}
	return consVal(v)
}

// LVal returns an LVal containing the cons data.
func (v ConsVal) LVal() LVal {
	return v.v
}

// ConsData returns the ConsData undersying v.
func (v ConsVal) ConsData() *ConsData {
	return v.v.Native.(*ConsData)
}

// CAR returns the head of list v.
func (v ConsVal) CAR() LVal {
	return v.data.CAR
}

// SetCAR stores w in the CAR of v.
func (v ConsVal) SetCAR(w LVal) {
	v.data.CAR = w
}

// CDR returns the tail of list v.
func (v ConsVal) CDR() LVal {
	return v.data.CDR
}

// SetCDR stores w in the CDR of v.
func (v ConsVal) SetCDR(w LVal) {
	v.data.CDR = w
}

// Len return the length of the list starting at v.
func (v ConsVal) Len() (int, bool) {
	cycle := make(map[*ConsData]bool)
	for n := 1; !cycle[v.data]; n += 1 {
		cycle[v.data] = true
		_v := v.CDR()
		if IsNil(_v) {
			return n, true
		}
		_, ok := GetCons(_v)
		if !ok {
			return n, false
		}
	}
	return -1, false
}

// Equal returns true if v2 is an identical cons.
func (v ConsVal) Equal(v2 LVal) bool {
	_v, ok := GetCons(v2)
	return ok && v.equal(_v)
}

func (v ConsVal) equal(v2 ConsVal) bool {
	return Equal(v.data.CAR, v2.data.CAR) && Equal(v.data.CDR, v2.data.CDR)
}

// DoEach iterates through cons cell in the sequence starting at v.
func (v ConsVal) DoEach(fn func(int, *ConsData) error) error {
	cycle := make(map[*ConsData]bool)
	var ok bool
	for n := 1; !cycle[v.data]; n += 1 {
		err := fn(0, v.data)
		if err != nil {
			return err
		}
		cycle[v.data] = true
		_v := v.CDR()
		if IsNil(_v) {
			return nil
		}
		v, ok = GetCons(_v)
		if !ok {
			return nil
		}
	}
	return nil
}

// Slice returns a slice of elements in the half-open interval [start, end).
// If slice encounters a non-list pair (cdr is not LNil) then false will be
// returned.  If the list terminates before reaching the end index then a
// shortened slice is returned.  Whatever portion of the list remains after
// reaching the end will be returned as tail.
//
// Slice will panic if end < start.
//
//		List(1, 2, 3, 4).Slice(1, 3) == []{2, 3}, List(4), true
//		List(1, 2).Slice(1, 3) == []{2}, Nil(), true
//		List(1).Slice(1, 3) == []{}, Nil(), true
//		Cons(1, Cons(2, Cons(3, 4))).Slice(1, 3) == []{2}, Cons(3, 4), false
//
// TODO Create a test for the above.
func (v ConsVal) Slice(start, end int) (s []LVal, tail LVal, ok bool) {
	for i := 0; i < start; i += 1 {
		tail = v.CDR()
		if IsNil(tail) {
			return nil, tail, true
		}
		v, ok = GetCons(tail)
		if !ok {
			return nil, tail, false
		}
	}
	return v.slice(end - start)
}

func (v ConsVal) slice(n int) (s []LVal, tail LVal, ok bool) {
	for i := 0; i < n; i++ {
		s = append(s, v.CAR())
		tail = v.CDR()
		if IsNil(tail) {
			return s, tail, true
		}
		v, ok = GetCons(tail)
		if !ok {
			// we want to fail here even if n == 1 and we already have 1
			// element because we encountered a cons pair that does not is not
			// a list.
			return s, tail, ok
		}
	}
	return s, tail, true
}

// SliceAll collects the list elemenst of v into a slice.  SliceAll returns
// false if v does not form a list but the returned slice will still include
// the CDR of the last cons.
func (v ConsVal) SliceAll() ([]LVal, bool) {
	ok := true
	var s []LVal
	v.DoEach(func(i int, data *ConsData) error {
		s = append(s, data.CAR)
		switch data.CDR.Type() {
		case LNil, LCons:
		default:
			s = append(s, data.CDR)
			ok = false
		}
		return nil
	})
	return s, ok
}

// IsList returns true if v is a true list -- if the CDR of its last cell is
// LNil.
func (v ConsVal) IsList() bool {
	cycle := make(map[*ConsData]bool)
	for {
		cycle[v.data] = true
		_v := v.CDR()
		if IsNil(_v) {
			return true
		}
		v, ok := GetCons(_v)
		if !ok {
			return false
		}
		if cycle[v.data] {
			return false
		}
	}
}

func (v ConsVal) format(w io.Writer, t symbol.Table) (int, error) {
	cw := lfmt.NewCountingWriter(w)
	var n int
	_, err := io.WriteString(cw, "(")
	if err != nil {
		return cw.N(), err
	}
	err = v.formatInner(cw, t)
	if err != nil {
		return n, err
	}
	_, err = io.WriteString(cw, ")")
	return cw.N(), err
}

func (v ConsVal) formatInner(w lfmt.CountingWriter, t symbol.Table) error {
	for {
		_, err := w.DeferCount(func(w io.Writer) (int, error) { return Format(w, v.data.CAR, t) })
		if err != nil || IsNil(v.data.CDR) {
			return err
		}
		if v.data.CDR.Type() == LCons {
			_, err = io.WriteString(w, " ")
			if err != nil {
				return err
			}
			v = consVal(v.data.CDR)
		} else {
			_, err = io.WriteString(w, " . ")
			if err != nil {
				return err
			}
			break
		}
	}
	_, err := Format(w, v.data.CDR, t)
	return err
}

type ListBuilder struct {
	init  bool
	front LVal
	back  *ConsData
}

func NewListBuilder(s Source) *ListBuilder {
	return &ListBuilder{}
}

// List returns a cons list with the elements appended so far.  If Append is
// called after Cons the value returned by cons will be modified.
func (b *ListBuilder) List() LVal {
	return b.front
}

// Append adds elements to the end of the cons list.
func (b *ListBuilder) Append(v ...LVal) {
	if len(v) == 0 {
		return
	}
	if !b.init {
		b.back = &ConsData{v[0], Nil()}
		b.front = makeCons(b.back)
		v = v[1:]
		b.init = true
	}
	for i := range v {
		data := &ConsData{v[i], Nil()}
		b.back.CDR, b.back = makeCons(data), data
	}
}

// ListIterator iterates through cons lists
type ListIterator struct {
	v    LVal
	rest LVal
	err  error
}

// NewListIterator returns a ListIterator that will iterate through list v.
func NewListIterator(v LVal) *ListIterator {
	return &ListIterator{
		v:    Nil(),
		rest: v,
	}
}

// Value returns the iteration's current value.  Value will return LNil if Next
// has not been called.
func (it *ListIterator) Value() LVal {
	return it.v
}

// Rest returns any items remaining to be iterated over
func (it *ListIterator) Rest() LVal {
	return it.rest
}

// Next advances the iterator to the next list element.  Next returns false if
// iteration terminated, either because the list had no more elements or
// because an non-list value was encountered.
func (it *ListIterator) Next() bool {
	if IsNil(it.rest) || it.err != nil {
		return false
	}
	if it.rest.Type() == LQuote {
		it.rest = it.rest.Native.(LVal)
	}
	if it.rest.Type() != LCons {
		it.err = fmt.Errorf("not a list: %v", it.rest.Type())
		return false
	}
	data := it.rest.Native.(*ConsData)
	it.v = data.CAR
	it.rest = data.CDR
	return true
}

// Err returns a non-nil error if the iteration encountered a non-list value
// terminating the cons chain.
func (it *ListIterator) Err() error {
	return it.err
}
