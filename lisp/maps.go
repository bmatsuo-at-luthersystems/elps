package lisp

import (
	"bytes"
	"fmt"
	"sort"
)

func sortedMapString(m *LVal) string {
	var buf bytes.Buffer
	buf.WriteString("(sorted-map")
	keys := sortedMapKeys(m)
	for _, key := range keys {
		buf.WriteString(" ")
		buf.WriteString(key.String())
		buf.WriteString(" ")
		buf.WriteString(mapGet(m, key).String())
	}
	buf.WriteString(")")
	return buf.String()
}

func sortedMapKeys(m *LVal) []*LVal {
	var ks mapKeys
	for k := range m.Map {
		key := sortedMapKey(k)
		if key == nil {
			panic("unable to serialize hashmap key")
		}
		ks = append(ks, key)
	}
	sort.Sort(ks)
	return ks
}

func mapGet(m *LVal, key *LVal) *LVal {
	k := toSortedMapKey(key)
	if k == nil {
		return Errorf("unhashable type: %s", key.Type)
	}
	v := m.Map[k]
	if v != nil {
		return v.Copy()
	}
	return Errorf("key not found: %s", key)
}

func mapSet(m *LVal, key *LVal, val *LVal) *LVal {
	k := toSortedMapKey(key)
	if k == nil {
		return Errorf("unhashable type: %s", key.Type)
	}
	m.Map[k] = val
	return Nil()
}

func toSortedMapKey(v *LVal) interface{} {
	switch v.Type {
	case LString:
		return v.Str
	case LSymbol:
		return mapSymbol(v.Str)
	case LInt:
		return v.Int
	case LFloat:
		return v.Float
	}
	return nil
}

func sortedMapKey(k interface{}) *LVal {
	switch v := k.(type) {
	case string:
		return String(v)
	case mapSymbol:
		return Symbol(string(v))
	case int:
		return Int(v)
	case float64:
		return Float(v)
	}
	return Error(fmt.Errorf("invalid key type: %T", k))
}

type mapSymbol string

type mapKeys []*LVal

func (ks mapKeys) Len() int { return len(ks) }

func (ks mapKeys) Swap(i, j int) {
	ks[i], ks[j] = ks[j], ks[i]
}

func (ks mapKeys) Less(i, j int) bool {
	if ks[i].IsNumeric() && ks[j].IsNumeric() {
		// TODO: Fall back to numeric sort here
	}
	if ks[i].Type != ks[j].Type {
		return ks[i].Type < ks[j].Type
	}
	switch ks[i].Type {
	case LString, LSymbol:
		return ks[i].Str < ks[j].Str
	case LInt:
		return ks[i].Int < ks[j].Int
	case LFloat:
		return ks[i].Float < ks[j].Float
	}
	// should not be reachable
	return false
}
