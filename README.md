#ELPS (Ellipse)

An embedded lisp system for Go programs.

##Build

```
go get -d ./...
make
```

##Usage

Launch an interactive REPL

```
$ elps repl
> (+ 3 1)
4
>^D
done
$
```

Run a program in a file

```
$ elps run prog.lisp
```

Embedded execution in a Go program

```
WIP -- See usage in elpstest/ and cmd/
```

##Reference

See the docs/ directory for more documentation:

- [Language reference](docs/lang.md)
