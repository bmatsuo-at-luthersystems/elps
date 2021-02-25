package symbol

// String returns the result of table.String(id) if id is present in table.
// String otherwise returns a diagnostic string describing id.
func String(id ID, table Table) string {
	s, _ := ResolveUnknown(defaultUnknownResolverFormat, table).Symbol(id)
	return s
}
