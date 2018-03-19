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

###Variable argument functions

A function's formal argument list may use the special symbol `&` before the
final argument to denote that the final argument should be bound to a list
containing all arguments not bound by previous argument symbols.

```lisp
(lambda (x & xs) (cons x (reverse xs)))
```

The above function can evaluate with one or more arguments.  The symbol `x`
will be bound to the first argument and `xs` will be bound to the remaining
(possibly empty) list of arguments.

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