BIN=../elps

EXAMPLES=$(shell find . -path ./metacircular -prune -false -o -name '*.lisp')

.PHONY: default
default: all

.PHONY: all test

.PHONY: wasm
all: wasm
wasm:
	$(MAKE) -C wasm

.PHONY: wasm-test
test: wasm-test
wasm-test:
	$(MAKE) -C wasm test

.PHONY: ${EXAMPLES}
all: ${EXAMPLES}
test: ${EXAMPLES}
${EXAMPLES}: ${BIN}
	${BIN} run $@

.PHONY: meta
test: meta
meta: ${BIN}
	${BIN} run ./metacircular/main.lisp

.PHONY: ${BIN}
${BIN}:
	$(MAKE) -C $(dir ${BIN}) build
