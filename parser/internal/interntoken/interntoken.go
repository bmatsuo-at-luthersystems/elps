package interntoken

import (
	"reflect"
	"sync"
	"unsafe"
)

type Table struct {
	mut    sync.RWMutex
	intern map[string]*string
}

func NewTable() *Table {
	return &Table{
		intern: make(map[string]*string),
	}
}

// GetBytes returns a string representing b.
func (tab *Table) GetBytes(b []byte) string {
	if tab == nil {
		return string(b)
	}
	// NOTE:  This code using reflect.StringHeader should be OK as long as
	// golang does not implement a moving garbage collector as unsafe.Pointers
	// could then lose track of referenced data.
	sliceHeader := (*reflect.SliceHeader)(unsafe.Pointer(&b))
	stringHeader := reflect.StringHeader{Data: sliceHeader.Data, Len: sliceHeader.Len}
	tab.mut.RLock()
	p, ok := tab.intern[*(*string)(unsafe.Pointer(&stringHeader))]
	tab.mut.RUnlock()
	if ok {
		return *p
	}
	// The bytes in b must be copied into a permanent location before being
	// inserted as a key in the table.
	return tab.insert(string(b))

}

// Get returns a string that equals s.
func (tab *Table) Get(s string) string {
	if tab == nil {
		return s
	}
	tab.mut.RLock()
	p, ok := tab.intern[s]
	tab.mut.RUnlock()
	if ok {
		return *p
	}
	return tab.insert(s)
}

func (tab *Table) insert(s string) string {
	tab.mut.Lock()
	p, ok := tab.intern[s]
	if !ok {
		p = &s
		tab.intern[s] = p
	}
	tab.mut.Unlock()
	return *p
}
