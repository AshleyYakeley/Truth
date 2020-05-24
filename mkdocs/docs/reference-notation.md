# Reference Notation

Reference notation makes working with references more convenient.
Reference notation is indicated by braces (`{}`).
Within reference notation, unreferences are indicated with percent (`%`).
For example:

`{"example"}` is the same as `pureRef "example"`  
`{"answer: " ++ %r}` is the same as `coMapRef (\v1 -> "answer: " ++ v1) r`  
`{%x + %(y ?? z)}` is the same as `applyRef (coMapRef (\v1 v2 -> v1 + v2) x) (y ?? z)`

If `expr : T`, then `{expr} : Ref +T`.  
If `ref : Ref {-P,+Q}`, then `%(ref) : Q` within reference notation.

Reference notation works only with the getting of references, and ignores setting.
Using `:=` or `delete` with references created this way will stop (see `stop` for what stopping means).
