# Do Notation

Do notation makes working with actions and other "monad" types more convenient.
Do-notation contains semicolon-separated do-lines inside `do.N` ... `end`, where `N` is a namespace.
`.N` may be omitted, in which case `Action.` is the namespace.

`N` must be a namespace with these names defined: `map`, `pure`, `ap`, `liftA2`, `**`, `>>`, `>>=`.
All of these names will be aliased from `N` into the current namespace.

Do-lines are expressions or of the form `<pattern> <- <expression>`.
The last line must be an expression.

* `do.N expr; exprs... end` is the same as `expr >>.N do.N exprs... end`
* `do.N pat <- expr; exprs... end` is the same as `expr >>=.N fn pat => do.N exprs... end`
* `do.N expr end` is the same as `expr`

Appropriate namespaces for do notation:

* `Maybe.`
* `List.`
* `Function.`
* `Either.`
* `Action.` (the default if `.N` is omitted)
