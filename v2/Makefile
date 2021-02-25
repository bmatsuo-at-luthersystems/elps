GO_TEST_FLAGS=-v -cover
GO_TEST=go test

# default runs a select group of targets
.PHONY: deafult

# build runs all the build targets without running tests
.PHONY: build
default: build

# test runs all the test targets (after a build)
.PHONY: test

# check runs a less chatty version of the tests.
.PHONY: check
default: check
check:
	${MAKE} test GO_TEST_FLAGS=

# coverage prints a code coverage report (not part of the test target)
.PHONY: coverage
coverage:
	${MAKE} test-go GO_TEST_FLAGS=-cover

.PHONY: datarace
datarace:
	${MAKE} test-go GO_TEST_FLAGS=-race

# benchmark runs all benchmarks (not part of the test target)
.PHONY: benchmark
benchmark:
	${MAKE} test-go GO_TEST_FLAGS="-test.bench=. -test.run=Benchmark"

.PHONY: build-cmd
build: build-cmd
build-cmd:
	@echo "nothing to build"

# test-go runs all the go tests
.PHONY: test-go
test: test-go
test-go:
	${GO_TEST} ${GO_TEST_FLAGS} ./...

# lisp-test runs all the lisp tests
.PHONY: test-lisp
test: test-lisp
test-lisp:
	@echo "no lisp tests to run"
