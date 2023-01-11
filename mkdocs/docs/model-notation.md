# Model Notation

Model notation makes working with whole models more convenient.
Model notation is indicated by braces (`{}`).
Within model notation, "unmodels" are indicated with percent (`%`).
For example:

`{"example"}` is the same as `return.WholeModel "example"`  
`{"answer: " ++ %r}` is the same as `coMap.WholeModel (fn v1 => "answer: " ++ v1) r`  
`{%x + %(y ?? z)}` is the same as `ap.WholeModel (coMap.WholeModel (fns v1 v2 => v1 + v2) x) (y ?? z)`

If `expr: T`, then `{expr}: WholeModel +T`.  
If `model: WholeModel {-P,+Q}`, then `%(model): Q` within model notation.

Model notation works only with the getting and updating of whole models, and ignores setting.
Using `:=` or `delete` with whole models created this way will stop (see `stop` for what stopping means).
