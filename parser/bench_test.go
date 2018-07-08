package parser_test

import (
	"path/filepath"
	"sort"
	"testing"

	"bitbucket.org/luthersystems/elps/elpstest"
	"bitbucket.org/luthersystems/elps/parser"
)

const fixtureDir = "testfixtures"

func BenchmarkParser(b *testing.B) {
	files, err := filepath.Glob(filepath.Join(fixtureDir, "*.lisp"))
	if err != nil {
		b.Fatalf("Failed to list test fixtures: %v", err)
	}
	sort.Strings(files) // should be redundant
	for _, path := range files {
		b.Run(filepath.Base(path), elpstest.BenchmarkParse(path, parser.NewReader))
	}
}
