package bootstrapparser

import (
	"bytes"
	"fmt"
	"io"
	"strings"

	lisp1 "github.com/luthersystems/elps/lisp"
	"github.com/luthersystems/elps/parser/rdparser"
	"github.com/luthersystems/elps/v2/pkg/lisp"
	"github.com/luthersystems/elps/v2/pkg/symbol"
)

// ParseBytes parses program as plaintext.
func ParseProgramBytes(name symbol.ID, table symbol.Table, program []byte) (lisp.LVal, error) {
	return ParseProgram(name, table, bytes.NewReader(program))
}

// ParseString parses program as plaintext.
func ParseProgramString(name symbol.ID, table symbol.Table, program string) (lisp.LVal, error) {
	return ParseProgram(name, table, strings.NewReader(program))
}

// Parse parses a plaintext program read from r and retruns the program text.
func ParseProgram(name symbol.ID, table symbol.Table, r io.Reader) (lisp.LVal, error) {
	strname, ok := table.Symbol(name)
	if !ok {
		return lisp.Nil(), fmt.Errorf("program name not found in symbol table")
	}
	reader := rdparser.NewReader()
	vals, err := reader.Read(strname, r)
	if err != nil {
		return lisp.Nil(), err
	}
	tmp := make([]lisp.LVal, len(vals))
	for i := range tmp {
		tmp[i], err = convert(vals[i], table)
		if err != nil {
			return lisp.Nil(), fmt.Errorf("program parse error: %w", err)
		}
	}
	return lisp.ListCompact(tmp...), nil
}

func convert(v *lisp1.LVal, table symbol.Table) (lisp.LVal, error) {
	if v.IsNil() {
		if v.Quoted {
			return lisp.Quote(lisp.Nil()), nil
		}
		return lisp.Nil(), nil
	}
	switch v.Type {
	case lisp1.LSymbol:
		id := table.Intern(v.Str)
		if v.Quoted {
			return lisp.Quote(lisp.Symbol(id)), nil
		}
		return lisp.Symbol(id), nil
	case lisp1.LInt:
		return lisp.Int(v.Int), nil
	case lisp1.LFloat:
		return lisp.Float(v.Float), nil
	case lisp1.LString:
		if v.Quoted {
			return lisp.Quote(lisp.String(v.Str)), nil
		}
		return lisp.String(v.Str), nil
	case lisp1.LSExpr:
		tmp := make([]lisp.LVal, len(v.Cells))
		for i := range tmp {
			var err error
			tmp[i], err = convert(v.Cells[i], table)
			if err != nil {
				return lisp.Nil(), fmt.Errorf("list cell %d: %w", i, err)
			}
		}
		if v.Quoted {
			return lisp.ListCompact(tmp...), nil
		}
		return lisp.ExprCompact(tmp...), nil
	case lisp1.LQuote:
		_v, err := convert(v.Cells[0], table)
		if err != nil {
			return lisp.Nil(), fmt.Errorf("quoted val: %w", err)
		}
		return lisp.Quote(_v), nil
	default:
		return lisp.Nil(), fmt.Errorf("invalid input type: %v", v.Type)
	}
}
