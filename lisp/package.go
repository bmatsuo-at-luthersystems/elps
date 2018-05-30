package lisp

// PackageRegistry contains a set of packages.
type PackageRegistry struct {
	Packages map[string]*Package
}

// NewRepository initializes and returns a new Repository.
func NewRepository() *PackageRegistry {
	return &PackageRegistry{
		Packages: make(map[string]*Package),
	}
}

func (r *PackageRegistry) DefinePackage(name string) {
	_, ok := r.Packages[name]
	if ok {
		return
	}
	r.Packages[name] = NewPackage(name)
}

// Package is a named set of bound symbols.  A package is interpreted code and
// belongs to the LEnv that creates it.
type Package struct {
	Name     string
	Symbols  map[string]*LVal
	FunNames map[string]string
}

// NewPackage initializes and returns a package with the given name.
func NewPackage(name string) *Package {
	return &Package{
		Name:     name,
		Symbols:  make(map[string]*LVal),
		FunNames: make(map[string]string),
	}
}

// Get takes an LSymbol k and returns the LVal it is bound to in pkg.
func (pkg *Package) Get(k *LVal) *LVal {
	v := pkg.get(k)
	if v.Type == LFun {
		// Set the function's name here in case the same function is defined
		// with multiple names.  We want to try and use the name the programmer
		// used.  The name may even come from a higher scope.
		pkg.FunNames[v.FID] = k.Str
	}
	return v
}

func (pkg *Package) get(k *LVal) *LVal {
	// LQSymbols are allowed...
	if k.Type != LSymbol && k.Type != LQSymbol {
		return Nil()
	}
	if k.Str == "t" {
		return Symbol("t")
	}
	v, ok := pkg.Symbols[k.Str]
	if ok {
		if v.Type == LFun {
			// Set the function's name here in case the same function is
			// defined with multiple names.  We want to try and use the name
			// the programmer used.
			pkg.FunNames[v.FID] = k.Str
		}
		return v.Copy()
	}
	return Errorf("unbound symbol: %v", k)
}

// GetFunName returns the function name (if any) known to be bound to the given
// FID.
func (pkg *Package) GetFunName(fid string) string {
	name, ok := pkg.FunNames[fid]
	if ok {
		return name
	}
	return ""
}

// Put takes an LSymbol k and binds it to v in pkg.
func (pkg *Package) Put(k, v *LVal) {
	if k.Type != LSymbol && k.Type != LQSymbol {
		return
	}
	if k.Str == "t" {
		panic("constant value")
	}
	if v == nil {
		panic("nil value")
	}
	if v.Type == LFun {
		pkg.FunNames[v.FID] = k.Str
	}
	pkg.Symbols[k.Str] = v.Copy()
}
