# Interfaces

Golang has shown some of the value interfaces can provide as a tool for
creating and using computational abstractions.  It is worth exploring if this
idea can be implemented in elps in a way that is highly ergonomic and also not
terribly slow.  This document will discuss possible ways that the feature might
be implemented and used in real code.

## Background

An interface allows for code to use a common api to perform a task where (parts
of) the underlying mechanism performing the task are abstracted as an
unnecessary detail(s).  Specifically, an interface abstracts details which are
irrelevant to the task at hand.  One unnecessary detail is the `type` of the
object performing the task.  As long as two types share a common api they
should be interchangeable for use in performing a task requiring that api, but
it is not yet well defined what it means for types to share an api.

### Sharing APIs Across Types

For two types to share an api they must first have the same set of api methods.
Naively, a method consists of a name, identifying the method, and a function
signature, specifying the arguments required for the method to perform its
function.  Languages with strict types or the ability to support multiple
return values would include additional information in the functions signature
(e.g. argument type signatures and return value signatures).  But on a
philosophical level sharing an api is a concept which transcends the abilities
of most if not all language type systems, such type systems cannot describe
computation adequately communicate the concept of sharing an api.  An api
encompasses the semantics and associations between method argument values,
method return values, and the interplay throughout a sequence of method calls
(or parallel sequences of method calls)

### Prior Works

Golang is the obvious base example of a language which has codified the concept
of interfaces at a core level.  As discussed previously, golang's type system
cannot fully capture the concept of sharing an api and the developers chose not
to address the issue directly.  Interfaces must document (through code
comments) any semantics are required of the interface api.  A golang the
interface itself is just a collection of method names and their function
signatures, which does match a naive intuition but is lacking in the ways
already discussed.  It is the job program to ensure that any time they supply
to implement a given interface will have the expected semantics (it is also the
library author's job to reduce the risk of programing errors but this is
outside the scope of the discussion for the time being).

Golang interfaces are a flexible type where a type implicitly "implements an
interface" merely by providing the same set of named methods with the
appropriate signatures.  The golang runtime is capable if determining if an
arbitrary value implements a given interface through runtime reflection (not
necessarily using the reflect package though).  But go is typically able to
determine implicit interface implementations at compile-time which reduces the
runtime overhead of using interfaces.

Java also introduced the concept of interfaces long ago.  One of the primary
differences between java interfaces and golang interfaces which people discuss
is that java required library authors to declare all the interfaces they
implement (i.e. a type cannot implement an interface its authors didn't know
about).

## Applications

It seems beneficial to perform an in-depth discussion of possible applications
for interface types in elps.  An interface in elps may be similar or
essentially identical to a corresponding interface in golang.  Though there
will also be interfaces which are more specific to computing in a lisp
environment. Nonetheless, it will be useful te discuss serveral interfaces
which have been particularly helpful in golang and examine why those interfaces
are useful or particularly well suited for the task they fullfil.  This will
help build a foundation with which we can define the goals and practical
limitations of an interface system which could be built in elps.

### Application: I/O

Some classic golang interfaces are io.Reader and io.Writer.  In golang, these
interfaces form the basis of most communication between a process and the
outside world and are, as such, quite core to the language as a whole.  But
beyond that they form the basis for many intra-process activities which include
communication (between goroutines) but they also act as a kind of common
denominator in separating sequential processing of IO operations (i.e.
buffering in-memory before dumping externally).


    package io

    // ...

    // Readers produce a stream of bytes which is captured by calling Read
    type Reader interface {
        Read(b []byte) (int, error)
    }

    // Writers consume a stream of bytes which is fed by calling Write
    type Writer interface {
        Write(b []byte) (int, error)
    }

An interesting observation of the two interfaces is that the methods, while
named differently, have identical type signatures.  This highlights that type
signatures are insufficient to capture the sameness of two apis.  The two
operations are reflections, backward mirror images, of each other.  They share
a type signature but perform opposing tasks, one modifies the contents of `b`
and the other (presumably) modifies itself or some wrapped object and leaves
`b` unmodified.  You could say that the abstracted operations of reading and
writing byte streams have a distinct orientation, a chirality -- the left and
right hand, identical through reflection but distinct and not interchangeable.

Obviously the names of the methods making up io.Reader and io.Writer are
different.  And that does imply they should have different semantics.  That is
simple enough and when two interfaces are designed by the same author(s) you
can expect the interface method names and the associated semantics to be
internally consistent amongst the package's interfaces.

However if independent groups of authors produce interfaces which share method
names and type signatures it is reasonable to assume that the interfaces are
likely not truly *the same*.   Interfaces which require signaling of events
like EOF may not use the same signal types.  And if an abstraction is at a high
enough level to require that the author make design decisions it is likely that
different authors could make different decisions while still producing the same
combination of method names and function signatures.  The addition of type
signatures in the function signature likely improves the overall safely of this
approach.  The authors of golang seem happy to deem this sufficiently safe when
coupled with safety guarantees from strict, static types.

The appropriateness of applying golang's design decision to elps will be more
completely discussed later.  But elps has neither strict nor static types.  And
as such the implications of naively applying golang's approach to implementing
and using interfaces in elps will be different.  Further, elps currently has no
native concept of IO outside of loading elps source files and debug printing of
raw lisp values.  The elps language currently has no concept of reading and
writing byte streams.

### Application:  String Formatting

An interface which is still indirectly related to I/O but is special enough to
deserve it's own discussion is fmt.Stringer.  This interface is so widely
implemented that it stands out from other I/O-adjacent interfaces.  The pattern
is so universally used many programmers would not know that its root definition
is in the fmt package.

    package fmt

    type Stringer interface {
        String() string
    }

The fmt.Stringer interface has proved invaluable for error reporting and
logging in golang applications.  It abstracts the language-level representation
of the data to a format for humans to read.  And by reading this text-based
abstraction of a value the observer gains a vastly improved understanding of
what value actually exists in the program's memory and what that implies about
its execution.

In elps, the issue of abstracting the presentation of data has largely been
avoided as the underlying language-level representation of types (as values
would be seen in source code) is often sufficient.  But there are a significant
number of examples where types are not conveniently representable with the
language-level representation (if they can even be represented as lisp source
code at all).  So there is indeed motivation to solve the problems that golang
is solving with the fmt.Stringer interface.

The elps language does currently provide a `format-string` function along with
other utilities which do allow for abstraction of primitively typed data (e.g.
strings, int, symbol).  But for complex structures like array, sorted-maps, and
user-defined types in particular the default representation may not always be
ideal.  And there are other values like function closures which are not
trivially representable and the default representation chosen in elps1 is
intentionally incomplete (lacking contextual scope details).  At some level
there is a need for user defined types in elps to (automatically) provide
better ways utilities like `format-string` to render them as text.

### Application:  Errors

Probably the most common interface in go is the `error` interface which is
defined as part of the language specification and does not belong to any
package.  Golang is notorious for it's error handling and the type appears
across the standard library and a large percentage of third-party packages.

    type error interface {
        Error() string
    }

The error interface actually evolved from an old pre-go1 interface, os.Error.
The interface was similar but had a distinct difference. 

    package os

    // ...

    type Error interface {
        String() string
    }

Aside from the name needing capitalization in order to be exported from the os
package, the name of the interface's sole method was in fact "String". The
interface was identical to fmt.Stringer.  This was convenient on one hand
because it meant all errors had a human readable representation when printed --
a property achieved for free based on the definitions of os.Error and
fmt.Stringer.

However, when golang's authors moved the concept of an `error` out of the os
package they also found that the property of os.Error and fmt.Stringer sharing
their set of methods and signatures was adequately problematic and they broke
everyone's gobeta code when moving to go1, deleting the os.Error type and
moving to the current `error` interface.  Golang will dynamically convert
values into any interface their method names and signatures satisfy.  And this
was such a core idea that the problem that the authors of golang did not want
to give it up, and had to grapple with their type system's complete inability
to detect the _semantic differences_ between an error and *anything else* which
also needed a human readable representation.  For this other reasons they
decided to introduce a distinction between errors and fmt.Stringer that could
be detected by the type system.  But the problem they encountered is more
universal and remains unsolved.  If authors use short method names and common
type signatures they are likely to produce interfaces which look the same to
golang's type system but have incompatible semantics.

It is unclear how much elps would benefit from an error handling model in which
all errors are merely implementations of an interface.  This application of
interfaces intended mostly to reinforce the notion that common type systems
cannot capture the notion of api sameness and provide a concrete example where
language authors had to deal with two identical apis which look the same.  This
could also be viewed as a case study in why interfaces are not the same as
sharing a common superclass in an objected oriented language.

### Other interesting and useful interfaces

TODO

- sort.Sort

- "iterator"

- "closers"  -- ties into looking at more purely functional approaches to solve
  the polymorphism problem.


## Possible Other Approaches

While this document has extolled the virtues of interfaces it should be fairly
obvious that not all successful programming languages have the concept of
interfaces and so they must provide other abstractions which, all together can
achieve the same high level goals of any application.  It must be emphasized
that most programming language would not implement the concepts of the above
example interfaces using a singular concept from that language.  Each language
has a particular style which suits it best and achieving the same high level
goals in different applications can look vastly different.  The classic example
here is the extreme differences in program structure between "classic"
imperative languages like C, Java, etc. with advanced functional languages like
haskell and ocaml.  Because lisp is a functional language it is worth exploring
the possibilities achievable in functional languages, while also keeping in
mind that many advanced functional languages also have strict, static type
systems.

## Goals

Because the type system in elps is not particularly advanced it makes sense to
also treat the language-level concept of an interface as a simple collection of
named methods paired with their function signatures.  This seems fairly
uncontroversial as golang has shown the effectiveness of this approach.  Even
though the approach dictates proper code documentation and api research on the
parts of the author and user, as discussed earlier, interfaces are often simple
enough to avoid dangerous misuse.  There are obviously exceptions, cases in
which "improper" use of an interface can lead to incorrect program behavior,
crashes, data corruption, and any other terrible type of consequence.  Given
golang's strict, static types and relatively excellent standard tooling the
language authors have been fairly comfortable of leaving the situation there.
The elps language has dynamic typing which is checked at runtime.  And if
nothing else, this would seem to increase the probability of crashing due to
interface misuse.  Presumably a higher chance of interface misuse would also
lead to increased error rates across all classifications of errors.  So this
apparent increase in risk would seem to suggest that elps should have
additional goals for an implementation of interface types to avoid writing
unreliable code.
