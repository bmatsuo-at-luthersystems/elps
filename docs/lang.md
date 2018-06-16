#Language Reference

##Basics

Lisp code interpreted by elps is given as a sequences of expressions encoded as
utf-8 text.

##Expressions

An expression is either an atom or a sequence of expressions delimited by
parenthesis `(...)`.  An environment evaluates one expression at a time,
evaluating each sub-expression before evaluating the main expression.
Expressions may also be *quoted* by prefixing it with an single quote `'`.
Quoted expressions are discussed in depth along with [expression
evaluation](#markdown-header-expression-evaluation).

##Atoms

Atoms can be symbols, numbers, and strings.

###Symbols

Symbols (identifiers) may consist of utf-8 letters, numbers, and many symbols
(symbols not used for language syntax).  Identifiers cannot start with a
number.

Symbols are currently case sensitive although this may change.

###Numbers

Numbers can be either int or floating point and will be converted between the
two forms as necessary.

###Strings

Strings are a sequence of utf-8 text delimited by double quotes `"`.  Strings
cannot contain line breaks.  A codepoint in a string may be escaped with a
preceding backslash `\`.

##Expression Evaluation

###Nil

The empty expression `()` is a special expression called "nil" and evaluates to
itself.  The value nil is used in the language to represent a false boolean
value (while anything non-nil represents a true boolean value).

###Atomic Expressions

Symbols evaluate to the last value bound to that symbol at the deepest
[scope](#markdown-header-scope) at which that symbol is bound.  Numbers and
strings evaluate to themselves.

###Quoted Expressions

Quoted expressions evaluate to themselves.  Quoted numbers and strings are
equivalent to their unquoted counterparts.  But a quoted nil value is not
equivalent to nil.

###Compound Expressions (Function Calls)

An expression containing sub-expressions is typically evaluated by evaluating
all its sub-expressions from left to right (top-down).  And then evaluating the
main expression by invoking the function resulting from the first
sub-experssion with arguments bound the results of remaining sub-expressions.

```lisp
(expr1 expr2 expr3)
```

In evaluating the above expression expr1 is evaluated first (and must evaluate
to a function).  Then expr2 is evaluated, followed by expr3.  A new scope is
created, binding expr1's function arguments to the values of expr2 and expr3,
and then the expr1's function in that scope.

##Functions

A function is a symbolic expression that utilizes some number of unbound
argument symbols.

```lisp
(lambda (x) (- x))
```

The above expression evaluates to an anonymous function (a lambda function)
which has one argument `x` and evaluates to the expression `(- x)` when x is
bound to a value through [expression
evaluation](#markdown-header-expression-evaluation).

```lisp
((lambda (x) (- x)) 3)  ; evaluates to -3 
```

The builtin macro `defun` is provided to bind names to functions.

```lisp
(defun neg (x) (- x))
(neg 3)                 ; evaluates to -3
```

###Optional function arguments

If a function's formal argument list contains the special symbol `&optional`
the following arguments are not required to call the function.

```lisp
(defun add1 (&optional x) (+ 1 (or x 0)))
```

The above function may be called with either zero or one argument.  If the
function is called without any arguments the optional argument x is bound to
the value nil.

```lisp
(add1)    ; evaluates to 1
(add1 2)  ; evaluates to 3
```

Functions can have multiple optional arguments.  Arguments are bound to
optional arguments in the order the arguments are defined and any left over
symbols are bound to the value nil.

```lisp
(defun add (&optional x y) (+ (or x 1) (or y 2)))
(add)     ; evaluates to 3
(add 2)   ; evaluates to 4
(add 2 0) ; evaluates to 2
```

There is no limit to the number of optional arguments a function can have. But if the number of optional arguments is too large it may be better to use keyward arguments instead.

###Variable argument functions

A function's formal argument list may use the special symbol `&rest` before
the final argument to denote that the final argument should be bound to a list
containing all arguments not bound by previous argument symbols.

```lisp
(defun cons-reverse (x &rest xs) (cons x (reverse xs)))
```

The above function can evaluate with one or more arguments.  The symbol `x`
will be bound to the first argument and `xs` will be bound to the remaining
(possibly empty) list of arguments.

```lisp
(cons-reverse 1)      ; evaluates to '(1)
(cons-reverse 1 2 3)  ; evaluates to '(1 3 2)
```

Variadic functions are prohibited from having keyword arguments due to
confusing semantics when mixing the two styles.  When keyword arguments are
needed avoid using `&rest` and just pass the variable argument as an
additional keyword argument.

###Keyward arguments

If a function's formal argument list contains the special symbol `&key` the
following symbols are keyword arguments.  Keyword arguments are like optional
arguments, in that they are not required to invoke the function.  Furthermore
they are bound to nil values when not provided.  However, keyword arguments
are unordered and when passed must be preceded by a keyword symbol indicating
which are follows.

```lisp
(defun point2d (&key x y) (list (or x 0) (or y 0)))
```

The above function defines two keyword arguments and may be called specifying
values for both, one, or neither.

```lisp
(point2d)           ; evaluates to '(0 0)
(point2d :y 1)      ; evaluates to '(0 1)
(point2d :x 1)      ; evaluates to '(1 0)
(point2d :y 1 :x 1) ; evaluates to '(1 1)
```

Keyword arguments are useful but they can also lead to some confusing errors.
Keywords are values.  And as values keywards can be passed to functions as
normal, required arguments.

```lisp
(defun single (x) (cons x ()))
(single :foo)  ; evaluates to '(:foo)
```

This unavoidable property of keyword arguments can lead to confusing runtime
errors when accidentally omitting required arguments or mixing keyword arguments and optional arguments.

**NOTE**: Due to the properties of keyword arguments it follows that a
function utilizing both optional and keyword arguments may only have values
bound to their keyword arguments once values have been bound to *all* optional
arguments.

###Currying (partial function evaluation)

If a function requires more arguments then are given it will evaluate to a
function of the remaining arguments.  Consider the following function.

```lisp
(defun add-pair (x y) (+ x y))
```

When `add-pair` is passed a single argument body expression `(+ x y)` still
cannot be evaluated because y cannot be bound to an argument value.  So a
function is returned which takes y as an argument and internally binds x to the
value previously given.

```lisp
(add-pair 1)       ; equivalent to (lambda (y) (+ 1 y))
((add-pair 1) 2)   ; evaluates to 3
```

###Unbound expressions

The builtin `expr` function allows for compact construction of simple
functions.

```lisp
(expr (+ % 1))      ; evaluates to (lambda (%) (+ % 1))
```

The special symbol `%` indicates an anonymous function argument.  Functions of
muiltple arguments can be defined by using the anonymous argument symbols `%1`,
`%2`, ... or the variadic anonymous argument `%&rest`.

```lisp
(expr (+ %1 %2))         ; evaluates to (lambda (%1 %2) (+ %1 %2))
(expr (reverse %&rest))  ; evaluates to (lambda (&rest %&rest) (reverse %&rest))
```

##Macros

A macro is a special function which receives unevaluated arguments (values are
not quoted, they just arn't evaluated). A macro function returns a quoted
expression which is subsequently evaluated in the scope of the original call.

##Special Operators

A special operator is like a macro, in that it receives unevaluated arguments,
but the result of a special operator will not be subsequently evaluated.
Examples of special operators are `if`, `lambda`, and `quasiquote`.  There is
no facility within the language for defining special operators.

##Scope

All symbol expressions are lexically scoped and resolve to the deepest binding
of that symbol.  Functions natually create a lexical scope that binds their
argument symbols.  The other way to create a lexical scope is through the use
of `let` and `let*` which take as their first argument a list of bindings
following by expressions which are executed in a nested scope containing those
bindings.

```lisp
(defun foo (x)
    (+ x 1))        ; x evaluates to the value bound in foo's scope

(let ((x 1))
    (+ x 1))        ; x evaluates to the value bound in the let's scope

(let ((x 1))
    (let ((y 2))
        (+ x 1)))   ; x evaluates to the value bound in the first let
```

If a function or `let` expression binds a symbol which was already bound in a
higher scope the symbol will be *shadowed* inside the `let` expression.

```lisp
(let ((x 1) (y 2))
    (defun add-y (x)    ; the argument x shadows the value bound by the let
        (+ x y))
    (defun add-x (y)    ; the argument y shadows the value bound by the let
        (+ x y))

(add-y 3)               ; evaluates to 5
(add-x 3)               ; evaluates to 4
```

The scope of a function is created when the function itself is created.  In the
above example the functions `add-y` and `add-x` always use values bound by
their arguments or by the let which contains the function definition

```lisp
(let ((x 10))
    (add-x 3))      ; still evaluates to 3
```

Macros must take care if they directly evaluate an argument that contains a
lambda (outside of qausiquote/unquote) because the resulting function will
inherit the scope of the macro and not the scope of the caller, which is
probably not desired.

##Data Structures

###Lists

The most primative data structure is a list, a quoted s-expression.

```lisp
'(1 2 3 4 "hello" ok)
```

Lists can be nested (it is not necessary to quote inner lists).

```lisp
'(1 2 (3 4 ()))
```

An empty list is equivalent to nil.

```lisp
(assert (nil? '()))
```

###Arrays

Arrays are references to continuous memory ranges.  The most common kind of
array is a vector -- a one dimensional array.  Zero dimensional arrays are a
reference to a single value.

```lisp
(vector 1 2 3)
```

Generally, functions in the standard library allow the programmer to specify
whether the output should be a vector or a list.

```lisp
(defun double (x) (* 2 x)
(map 'vector double '(1 2 3))      ; evaluates to (vector 2 4 6)
(map 'list double (vector 1 2 3))  ; evaluates to '(2 4 6)
```

###Sorted Maps

A sorted map is a mapping between keys and values which ensures that key
traversal is always done in sorted, increasing order.  Sorted maps can contain
keys that are either symbols or strings.  Looking up values by key can be done
with either a string or a symbol, regardless which type was used to insert/set
the value originally.

```lisp
(let ((m (sorted-map 'alice 0 'bob 1 'carol 2)))
    (get m "carol"))    ; evaluates to 2
```

Maps are mutable values and can be updated with the `assoc!` function.

```lisp
(let ((m (sorted-map 'alice 0 'bob 1)))
    (assoc! m 'carol 2)
    (get m 'carol))     ; evaluates to 2
```

##Packages

Packages allow namespace isolation for components of a codebase as its
complexity increases.

### Basics

Packages are created/modified using the `in-package`
function, which changes the environment's working package.  Symbols bound using
`set`, `defun`, `defmacro`, etc will be bound in the working package.

```lisp
(in-package 'my-new-package)
(export 'my-special-function)
(defun my-special-function () (debug-print "something special"))
(defun my-other-function () (debug-print "something else"))
```

Outside of the `my-new-package` package, the symbol `my-special-function` may
be bound to other values.  Value defined inside `my-new-package` may be
accessed by qualifying the symbol using the package name.

```lisp
(my-new-package:my-special-function)  ; prints "something special"
(my-new-package:my-other-function)    ; prints "something else"
```

### Importing symbols

Symbols exported within a package may be imported to another package with the
`use-package` function.

```lisp
(in-package 'my-other-package)
(use-package 'my-new-package)
(my-special-function)           ; prints "something special"
```

In the above example, `my-special-function` becomes bound in
`my-other-package`.  But the symbol `my-other-function` remains unbound because
it was never exported.  If you really wanted to bind `my-other-function` it
would be possible by using a qualified symbol.

```lisp
(set 'my-other-function my-new-package:my-other-function)
```

**NOTE:** All packages use the "lisp" package, which defines all of the
language builtin functions and macros.  It is not currently possible to change
this behavior for packages defined by lisp code.  Embedded lisp isntances are
able change this behavior globally -- something outside the scope of this
document.

### Standard library

A default lisp instance will have a standard set of packages available outside
of the language base "lisp" package.  There are packages for working with time,
json, stream encodings, math, etc.  These packages generally have simple, short
names.

```lisp
(set 'now (time:utc-now))
(debug-print (time:format-rfc3339 now))
```

### User packages

For packages outside of the standard library it in recommended that names use a
URL format for organizational clarity and to avoid package name collisions.

```lisp
(in-package 'example.com/faster-json)
(use-package 'example.com/faster-json/utils)
```
