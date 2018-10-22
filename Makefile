BIN=./elps
GO_FILES=$(shell find . -name '*.go')

.PHONY: default
default: build
	@

.PHONY: repl
repl: build
	${BIN} repl

.PHONY: test
test:
	GOCACHE=off go test -cover ./...
	$(MAKE) examples

.PHONY: examples
examples:
	$(MAKE) -C examples

.PHONY: install
install:
	go install

.PHONY: build
build: ${BIN}
	@

.PHONY: clean
clean:
	rm -f ${BIN}

${BIN}: ${GO_FILES}
	go build
