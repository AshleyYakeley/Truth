# Implicit Parameters

You can create an implicit parameter using a question mark followed by an lname, for example, `?store`, `?gtk`, `?myParam` etc.

Expressions can include implicit parameters, and they can be discharged with the `imply` keyword.

For example:
```pinafore
let
    x = ?y + 1;
in
imply ?y = 2 in x + 3 
```

Note that the type of an expression with an implicit parameter does not mention the parameter.
