BIN=../elps

EXAMPLES=$(shell find . -name '*.lisp')

.PHONY: default
default: all

.PHONY: all
all: ${EXAMPLES}

.PHONY: ${EXAMPLES}
${EXAMPLES}: ${BIN}
	${BIN} run $@

.PHONY: ${BIN}
${BIN}:
	$(MAKE) -C $(dir ${BIN}) build
