# Memory Management

This document describes how elps manages its allocated storage.  It is a
companion to the language embedding reference, which attempts to remain light
on implementation details but should provide the most important information for
integrators and application developers.

## Summary

This document defines some high-level concepts regarding values and memory in
lisp.  Further, it discusses how values interact with with the Go garbage
collector and optimizations that are made to reduce memory pressure from
constantly allocating `LVal` values.  Finally, the document describes a
protocol for builtin functions interacting with `*LVal` types that is
**critically important** for maintaining the integrity of computations carried
out by an environment.  Beyound encountering runtime errors, embedding
applications which do not comply with the protocol described here risk data
races, runtime panics/segfaults, memory corruption, raptors, and other
unspeakable horrors.

## Motivation

All lisp values are represented in Go as an `LVal` struct value.  This is
largely due done for simplicity and due to the interpreter's roots in the BYOL
book.  The implementation allows, for instance, all values to be associated
with a location in source code (file and line number) with relatively low
complexity.  In addition, this design also allows builtin functions to operate
effeciently on primitive values without the Go compiler forcing runtime excess
runtime checks.  However, a naive implementation taking this approach puts a
lot of pressure on the Go runtime's garbage collector, which makes the
intepreter signficantly slower than an equivalent intepreter written in C.  In
a laguage like C which has manual memory management strict ownership semantics
are required to program an intepreter like this without encountering persistent
memory leaks -- which, in turn, will drastically improve memory performance.
In Go, it is not desirable for application developers to perform manual memory
allocation/deallocation.  However, ownership semantics are still required in Go
because lisp values are largely meant to appear immutable and a builtin
function that fails to respect ownership semantics may result in code which
does not behave as expected.

## Problem Statment

The language needs rules for application developers to follow which allow for
safe, low-friction development of embedded DSLs.  The rules for application
developers must allow for `*LVal` types to be "recycled" so the intepreter does
not always need to allocate an `LVal` when creating a new value.  An ideal
solution would allow tests to easily catch a builtin function's failure to
follow the protocol, though this may not be possible.
