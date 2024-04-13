# Expressions & Patterns

## Expressions

In Pinafore, the main program is a single expression.
These are the different kind of expressions:

* *string literals*: `"abc\ndef"`. Use backslash for certain escapes.
* *numeric literals*: `34.5`, `~34.5`. The first is a rational, the second is an inexact number (internally represented as a double).
* *names*: `x`. These are bound by declarators (e.g `let`) and patterns in function bindings (`fn` and `match`).
* *functions*: `fn x => x + 2 * x`, `match Just x => x; None => 0; end`. These are constructed by `fn` (which have one "case", matching a pattern to an expression) or by `match` (with multiple cases)
* *if expressions*: `if t then "yes" else "no"`.
* [*declarated expressions*](declarations.md): `let x = 3 in x + x`, `with N in x`. The declarator modifies the expressions to pass declarations.
* [*do-notation*](do-notation.md): `do x <- f; return x + x; end`. This notation makes it easier to work with monad-like types.
* [*applicative notation*](applicative-notation.md): `{.List %x + %y}`. This notation makes it easier to work with applicative-like types.
* *function applications*: `sort mylist`. This is applying an expression (the function) to an expression (the argument).
* *list constructors*: `[1,2,3]`.
* *tuple constructors*: `(1,"xyz",())`. Note that there are no tuple types larger than pairs, so this is equivalent to `(1,("xyz",()))`.

## Patterns

Patterns are for matching against values in function cases (`fn` and `match`) and in `let`-bindings.

* An underscore matches everything.
* A lower-case name matches everything, and binds to the matched value.
* A capitalised name (followed by patterns) matches a data constructor.
* The form `P@Q` matches when both patterns `P` and `Q` match.
* The form `P : T` matches when `P` matches. Pinafore will reject this if pattern `P` cannot be subsumed to type `T`.
* The form `P :? T` matches when the value can be [dynamically cast](dynamic-supertypes.md) to `T`, and `P` matches that.
* The form `P as N` matches when `P` matches. Names matched in `P` will be put in [namespace](declarations.md#namespaces-amp-namespace-declarations) `N`.
