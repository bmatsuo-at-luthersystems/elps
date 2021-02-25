package symbol

import "sync/atomic"

// An ID is the combination of a package id and the id of a symbol in that
// package.
type ID uint64

const (
	// MaxID is the largest ID that can exist within a package.
	MaxID = 0x00000000FFFFFFFF
	// PkgMask is used to extract the package ID from an ID
	PkgMask = 0xFFFFFFFF00000000
)

func (id ID) PkgID() ID {
	return id & PkgMask
}

// IDGen is a function that generates unique IDs.
type IDGen interface {
	// NewID returrns a unique ID.  It is not specified at the interface level
	// what IDs are returned, only that they are unique.
	NewID() ID
}

// NewIDGen returns a basic IDGen that will generate unique ids from min to
// MaxID.  The returned IDGen will not produce the value min.
func NewIDGen(min ID) IDGen {
	if min > MaxID {
		panic("invalid min ID")
	}
	return &gen{lastid: min}
}

type gen struct {
	lastid ID
}

var _ IDGen = (*gen)(nil)

func (g *gen) NewID() ID {
	id := atomic.AddUint64((*uint64)(&g.lastid), 1)
	if id > MaxID {
		panic("too many ids generated")
	}
	return ID(id)
}

// String is equivalent to calling String(id, DefaultGlobalTable).
func (id ID) String() string {
	return String(id, DefaultGlobalTable)
}
