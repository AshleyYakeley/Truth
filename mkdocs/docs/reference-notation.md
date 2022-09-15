# Reference Notation

Reference notation makes working with whole references more convenient.
Reference notation is indicated by braces (`{}`).
Within reference notation, unreferences are indicated with percent (`%`).
For example:

`{"example"}` is the same as `pureWhole "example"`  
`{"answer: " ++ %r}` is the same as `coMapWhole (fn v1 => "answer: " ++ v1) r`  
`{%x + %(y ?? z)}` is the same as `applyWhole (coMapWhole (fns v1 v2 => v1 + v2) x) (y ?? z)`

If `expr: T`, then `{expr}: WholeRef +T`.  
If `ref: WholeRef {-P,+Q}`, then `%(ref): Q` within reference notation.

Reference notation works only with the getting of whole references, and ignores setting.
Using `:=` or `delete` with whole references created this way will stop (see `stop` for what stopping means).
