# Reference Notation

Reference notation makes working with references more convenient.
Reference notation is indicated by braces (`{}`).
Within reference notation, unreferences are indicated with percent (`%`).
For example:

`{"example"}` is the same as `pureref "example"`  
`{"answer: " ++ %r}` is the same as `comapref (\v1 -> "answer: " ++ v1) r`  
`{%x + %(y ?? z)}` is the same as `applyref (comapref (\v1 v2 -> v1 + v2) x) (y ?? z)`

If `expr :: T`, then `{expr} :: Ref +T`.  
If `ref :: Ref {-P,+Q}`, then `%(ref) :: Q` within reference notation.

Reference notation works only with the getting of references, and ignores setting.
Using `:=` or `delete` with references created this way is allowed but does nothing.
