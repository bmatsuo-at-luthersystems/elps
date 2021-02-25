package symbol

import (
	"fmt"
	"sort"
	"sync"
)

// DefaultGlobalTable is the default symbol table.  It should be used by
// processes to initialize symbols during package init to create fixed handles
// to symbols.  Applications may use DefaultGlobalTable or export its rows to
// create a copy before use if the copy is short-lived but may have junk values
// interned.
var DefaultGlobalTable Exporter = NewGlobalTable()

// Intern uses DefaultGlobalTable to intern s and returns its ID.
func Intern(s string) ID {
	return DefaultGlobalTable.Intern(s)
}

// Table maps symbol IDs to strings.
type Table interface {
	// Len returns the number of symbols interned in the table.
	Len() int
	// Intern inserts the given symbol into the table if it is not present and
	// returns its ID.
	Intern(symbol string) ID
	// Peek retrieves the ID of a symbol without automatically interning it.
	// Peek returns true iff the symbol has been interned into the table.
	Peek(symbol string) (ID, bool)
	// Symbol returns the symbol associated with id.
	Symbol(id ID) (string, bool)
	// Package returns a package in the given ID space.  Package should not be
	// called multiple times with the same p.  Package panics if p is greater
	// than MaxID or does not exist in the Table.
	Package(p ID) Package
}

// ResolveUnknown returns a Table that returns diagnostic strings when the
// method Symbol is passed an unknown symbol ID.  The Symbol method on the
// returned Table will always return true and will use fmt.Sprintf to create a
// string representing any symbols unknown to t.  All other methods on the
// returned Table proxy the corresponding methods on t.
func ResolveUnknown(format string, t Table) Table {
	return newUnknownResolver(format, t)
}

const defaultUnknownResolverFormat = "#<SYMBOL %#x>"

type unknownResolver struct {
	format string
	Table
}

func newUnknownResolver(format string, t Table) *unknownResolver {
	if format == "" {
		format = defaultUnknownResolverFormat
	}
	return &unknownResolver{format, t}
}

// Symbol overrides t.Table.Symbol and use the t.format to describe unknown
// strings.  Symbol always returns true.
func (t *unknownResolver) Symbol(id ID) (string, bool) {
	s, ok := t.Table.Symbol(id)
	if ok {
		return s, true
	}
	return fmt.Sprintf(t.format, uint64(id)), true
}

// Package is a namespace of symbols
type Package interface {
	// InternID returns a qualified ID corresponding to the unqualified id.
	// InternID panics if passed a qualified id.
	InternID(id ID) ID
	// PeekID returns the qualified ID corresponding to the unqualified id.
	// PeekID returns false if the unqualified id has not been interned.
	// PeekID panics if passed a qualified id.
	PeekID(id ID) (ID, bool)
}

// Exporter is a table that can dump its symbol mapping.
type Exporter interface {
	Table
	// Export returns a slice containing all table data which can be used to
	// bootstrap a new Table.
	Export() []TableRow
}

// BulkIntern is a table that can insert multiple symbols.
type BulkInterner interface {
	Table
	// InternAll performs a bulk Intern operation and returns a list of IDs
	// that matches the given symbols.
	InternAll(...string) []ID
}

func InternAll(t Table, symbols ...string) []ID {
	switch t := t.(type) {
	case BulkInterner:
		return t.InternAll(symbols...)
	default:
		ids := make([]ID, 0, len(symbols))
		for _, s := range symbols {
			ids = append(ids, t.Intern(s))
		}
		return ids
	}
}

func NewGlobalTable() Exporter {
	return newTable()
}

func NewTable(rows ...TableRow) Table {
	return newTable(rows...)
}

// CopyGlobalTable is equivalent to NewTable(DefaultGlobalTable.Export()...)
func CopyGlobalTable() Exporter {
	return newTable(DefaultGlobalTable.Export()...)
}

type TableRow struct {
	Symbol string
	ID     ID
}

func sortTableRowBySymbol(r []TableRow) {
	sort.Sort((*tableRowBySymbol)(&r))
}

type tableRowBySymbol []TableRow

func (r *tableRowBySymbol) Len() int           { return len(*r) }
func (r *tableRowBySymbol) Less(i, j int) bool { return (*r)[i].Symbol < (*r)[j].Symbol }
func (r *tableRowBySymbol) Swap(i, j int)      { (*r)[i], (*r)[j] = (*r)[j], (*r)[i] }

// table is a generic table
type table struct {
	sync sync.RWMutex
	g    IDGen
	i    map[ID]string
	s    map[string]ID
}

var (
	_ Table        = (*table)(nil)
	_ BulkInterner = (*table)(nil)
	_ Exporter     = (*table)(nil)
)

func newTable(r ...TableRow) *table {
	t := &table{
		i: make(map[ID]string),
		s: make(map[string]ID),
	}
	var min ID
	for i := range r {
		t.i[r[i].ID] = r[i].Symbol
		t.s[r[i].Symbol] = r[i].ID
		if r[i].ID > min {
			min = r[i].ID
		}
	}
	t.g = NewIDGen(min)
	return t
}

// Len implements the Table interface
func (t *table) Len() int {
	return len(t.s)
}

// Export implements the Exporter interface
func (t *table) Export() []TableRow {
	t.sync.RLock()
	defer t.sync.RUnlock()
	r := make([]TableRow, 0, len(t.s))
	for sym, id := range t.s {
		r = append(r, TableRow{
			Symbol: sym,
			ID:     id,
		})
	}
	sortTableRowBySymbol(r)
	return r
}

// Intern implements the Table interface
func (t *table) Intern(s string) ID {
	t.sync.Lock()
	defer t.sync.Unlock()
	return t.intern(s)
}

// InternAll implements the BulkInterner interface
func (t *table) InternAll(s ...string) []ID {
	ids := make([]ID, 0, len(s))
	t.sync.Lock()
	defer t.sync.Unlock()
	for _, s := range s {
		ids = append(ids, t.intern(s))
	}
	return ids
}

func (t *table) intern(s string) ID {
	if id, ok := t.s[s]; ok {
		return id
	}
	id := t.g.NewID()
	t.s[s] = id
	t.i[id] = s
	return id
}

// Peek implements the Table interface
func (t *table) Peek(s string) (ID, bool) {
	t.sync.RLock()
	defer t.sync.RUnlock()
	id, ok := t.s[s]
	return id, ok
}

// Symbol implements the Table interface
func (t *table) Symbol(id ID) (string, bool) {
	t.sync.RLock()
	defer t.sync.RUnlock()
	s, ok := t.i[id]
	return s, ok
}

// Package implements the Table interface
func (t *table) Package(p ID) Package {
	if p > MaxID {
		panic("invalid package id")
	}
	t.sync.RLock()
	defer t.sync.RUnlock()
	if _, ok := t.i[p]; !ok {
		panic("package symbol does not exist")
	}
	return newPackage(t, p)
}

type pkg struct {
	sync sync.RWMutex
	t    *table
	p    ID
	q    map[ID]ID
}

var _ Package = (*pkg)(nil)

func newPackage(t *table, p ID) *pkg {
	return &pkg{
		t: t,
		p: p,
		q: make(map[ID]ID),
	}
}

// InternID implements the Package interface
func (p *pkg) InternID(id ID) ID {
	if id&PkgMask != 0 {
		panic("package cannot intern a qualified symbol")
	}
	p.sync.Lock()
	defer p.sync.Unlock()

	if q, ok := p.q[id]; ok {
		return q
	}

	base, ok := p.t.Symbol(id)
	if !ok {
		panic("no symbol corresponding to id")
	}
	pkg, ok := p.t.Symbol(p.p)
	if !ok {
		panic("no symbol corresponding to package")
	}
	s := fmt.Sprintf("%s:%s", pkg, base)
	q := p.t.Intern(s)
	p.q[id] = q
	return q
}

// PeekID implements the Package interface
func (p *pkg) PeekID(id ID) (ID, bool) {
	if id&PkgMask != 0 {
		panic("package cannot intern a qualified symbol")
	}
	p.sync.RLock()
	defer p.sync.RUnlock()

	q, ok := p.q[id]
	return q, ok
}
