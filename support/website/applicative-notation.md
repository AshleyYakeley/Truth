# Applicative Notation

Applicative notation makes working with whole models and other "applicative" types more convenient.
Applicative notation is indicated by `ap` and braces (`{}`), and optionally a namespace `.N`.
If the namespace is omitted, `WholeModel.` is used.

Within applicative notation, "applications" are indicated with percent (`%`).
For example:

* `ap.N {"example"}` is the same as `pure.N "example"`  
* `ap.N {"answer: " ++ %r}` is the same as `map.N (fn v1 => "answer: " ++ v1) r`  
* `ap.N {%x + %(y ?? z)}` is the same as `ap.N (map.N (fn v1, v2 => v1 + v2) x) (y ?? z)`

`N` must be a namespace with these names defined: `map`, `pure`, `ap`, `liftA2`, `**`, `>>`.
All of these names will be aliased from `N` into the current namespace.

If the names in `N` have the appropriate types, then these will be true:

* If `expr: T`, then `ap.N{expr}: N +T`.  
* If `n: N (-P,+Q)` or `n: N Q`, then `%n: Q` within applicative notation for namespace `N`.

When used with the `WholeModel` namespace, applicative notation works only with the getting and updating of whole models, and ignores setting.
Using `:=` or `delete` with whole models created this way will stop (see `stop` for what stopping means).

Appropriate namespaces for applicative notation:

* `WholeModel.` (the default if `.N` is omitted)
* `Maybe.`
* `List.`
* `List1.List.`
* `Function.`
* `Either.`
* `Action.`
* `Task.`
* `Drawing.Cairo.`
