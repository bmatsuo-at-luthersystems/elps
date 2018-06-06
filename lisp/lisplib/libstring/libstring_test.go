// NOTE:  This file uses package name suffixed with _test to avoid an import
// cycle.  packages outside the standard library shouldn't need to use a _test
// suffix in their test files.
package libstring_test

import (
	"testing"

	"bitbucket.org/luthersystems/elps/elpstest"
)

func TestPackage(t *testing.T) {
	r := &elpstest.Runner{}
	r.RunTestFile(t, "string_test.lisp")
}
