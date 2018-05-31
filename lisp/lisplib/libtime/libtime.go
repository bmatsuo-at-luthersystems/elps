package libtime

import (
	"time"

	"bitbucket.org/luthersystems/elps/lisp"
)

// DeafultPackageName is the package name used by LoadPackage.
const DefaultPackageName = "time"

// LoadPackage adds the time package to env
func LoadPackage(env *lisp.LEnv) *lisp.LVal {
	name := lisp.Symbol(DefaultPackageName)
	e := env.DefinePackage(name)
	if !e.IsNil() {
		return e
	}
	e = env.InPackage(name)
	if !e.IsNil() {
		return e
	}
	for _, fn := range builtins {
		env.AddBuiltins(true, fn)
	}
	return lisp.Nil()
}

// Time creates an LVal representing the time t.
func Time(t time.Time) *lisp.LVal {
	return lisp.Native(t)
}

// GetTime gets a time.Time value from v and returns it.
func Get(v *lisp.LVal) (time.Time, bool) {
	t, ok := v.Native.(time.Time)
	return t, ok
}

// Duration returns an LVal representing duration d.
func Duration(d time.Duration) *lisp.LVal {
	return lisp.Native(d)
}

// GetDuration gets a time.Duration value from v and returns it.
func GetDuration(v *lisp.LVal) (time.Duration, bool) {
	d, ok := v.Native.(time.Duration)
	return d, ok
}

type builtin struct {
	name    string
	formals *lisp.LVal
	fun     lisp.LBuiltin
}

func (fun *builtin) Name() string {
	return fun.name
}

func (fun *builtin) Formals() *lisp.LVal {
	return fun.formals
}

func (fun *builtin) Eval(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	return fun.fun(env, args)
}

var builtins = []*builtin{
	{"utc-now", lisp.Formals(), BuiltinUTCNow},
	{"format-rfc3339", lisp.Formals("datetime"), BuiltinFormatRFC3339},
	{"format-rfc3339-nano", lisp.Formals("datetime"), BuiltinFormatRFC3339Nano},
	{"sub", lisp.Formals("end", "start"), BuiltinSub},
	{"duration", lisp.Formals("time-duration"), BuiltinDurationSeconds},
	{"duration-ms", lisp.Formals("time-duration"), BuiltinDurationMS},
	{"duration-ns", lisp.Formals("time-duration"), BuiltinDurationNS},
}

func BuiltinUTCNow(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	return Time(time.Now().UTC())
}

func BuiltinFormatRFC3339(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	lt := args.Cells[0]
	if lt.Type != lisp.LNative {
		return env.Errorf("argument is not a time: %v", lt.Type)
	}
	t, ok := lt.Native.(time.Time)
	if !ok {
		return env.Errorf("argument is not a time: %v", lt)
	}
	return lisp.String(t.Format(time.RFC3339))
}

func BuiltinFormatRFC3339Nano(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	lt := args.Cells[0]
	if lt.Type != lisp.LNative {
		return env.Errorf("argument is not a time: %v", lt.Type)
	}
	t, ok := lt.Native.(time.Time)
	if !ok {
		return env.Errorf("argument is not a time: %v", lt)
	}
	return lisp.String(t.Format(time.RFC3339Nano))
}

func BuiltinSub(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	lt1, lt2 := args.Cells[0], args.Cells[1]
	if lt1.Type != lisp.LNative {
		return env.Errorf("argument is not a time: %v", lt1.Type)
	}
	if lt2.Type != lisp.LNative {
		return env.Errorf("argument is not a time: %v", lt2.Type)
	}
	t1, ok := lt1.Native.(time.Time)
	if !ok {
		return env.Errorf("argument is not a time: %v", lt1)
	}
	t2, ok := lt2.Native.(time.Time)
	if !ok {
		return env.Errorf("argument is not a time: %v", lt2)
	}
	return Duration(t1.Sub(t2))
}

// BulitinDurationSecods returns a float equal to the the number of seconds in
// the given duration.
func BuiltinDurationSeconds(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	lt := args.Cells[0]
	if lt.Type != lisp.LNative {
		return env.Errorf("argument is not a duration: %v", lt.Type)
	}
	d, ok := lt.Native.(time.Duration)
	if !ok {
		return env.Errorf("argument is not a duration: %v", lt)
	}
	// TODO:  Check for overflow and use a method less prone to overflow.
	return lisp.Float(float64(d) / float64(time.Second))
}

// BuiltinDuriationNS returns a float equal to the the number of nanoseconds in
// the given duration.
func BuiltinDurationMS(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	lt := args.Cells[0]
	if lt.Type != lisp.LNative {
		return env.Errorf("argument is not a duration: %v", lt.Type)
	}
	d, ok := lt.Native.(time.Duration)
	if !ok {
		return env.Errorf("argument is not a duration: %v", lt)
	}
	// TODO:  Check for overflow and use a method less prone to overflow.
	return lisp.Float(float64(d) / float64(time.Millisecond))
}

// BulitinDuriationNS returns an integer equal to the the number of nanoseconds
// in the given duration.
func BuiltinDurationNS(env *lisp.LEnv, args *lisp.LVal) *lisp.LVal {
	lt := args.Cells[0]
	if lt.Type != lisp.LNative {
		return env.Errorf("argument is not a duration: %v", lt.Type)
	}
	d, ok := lt.Native.(time.Duration)
	if !ok {
		return env.Errorf("argument is not a duration: %v", lt)
	}
	if int64(int(d)) != int64(d) {
		return env.Errorf("duration is too large")
	}
	return lisp.Int(int(d))
}
