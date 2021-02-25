package runtime

import "fmt"

// Signal is a control value sent to a Thread
type Signal uint32

const (
	SIG_NONE Signal = iota
	SIG_INTERRUPT
	SIG_DUMPSTACK
	SIG_INVALID
)

var signalStrings = []string{
	SIG_NONE:      "NONE",
	SIG_INTERRUPT: "INTERRUPT",
	SIG_DUMPSTACK: "DUMPSTACK",
	SIG_INVALID:   "INVALID",
}

func (s Signal) String() string {
	if s >= SIG_INVALID {
		return fmt.Sprintf("%s(%x)", signalStrings[SIG_INVALID], uint32(s))
	}
	return signalStrings[s]
}

// SignalError is an error resulting from a thread receiving a signal.
type SignalError Signal

func (err SignalError) Error() string {
	return Signal(err).String()
}
