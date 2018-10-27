BIN=../elps

EXAMPLES=$(shell find . -name '*.lisp')

.PHONY: default
default: all

.PHONY: all
all: ${EXAMPLES}

.PHONY: wasm
all: wasm
wasm:
	$(MAKE) -C wasm


.PHONY: ${EXAMPLES}
${EXAMPLES}: ${BIN}
	${BIN} run $@

.PHONY: ${BIN}
${BIN}:
	$(MAKE) -C $(dir ${BIN}) build
