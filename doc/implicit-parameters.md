# Implicit Parameters

An implicit parameter is a kind of free variable of an expression, that can be bound at a later point using `imply` rather than under its own lexical scope.
You can create an implicit parameter using a question mark followed by an lname, for example, `?store`, `?gtk`, `?myParam` etc.
Expressions can include implicit parameters, and they can be discharged with the `imply` keyword.

For example:
```pinafore
let
    x = ?y + 1;
in
imply ?y = 2 in x + 3 
```

## Type

Using `:type` in Pinafore's interactive mode will show the type of an expression, along with the names and types of all free variables.
For example:

```
pinafore> :type 3
: Integer
pinafore> :type ?x + 3
: ?x: Integer, Integer
pinafore> :type x + 3
: x: Integer, Integer
```

However, this form is only for presentation. The types of implicit parameters or other free variables cannot be specified in type signatures.

```
pinafore> :type ?x : Integer
: ?x: Integer, Integer
pinafore> :type ?x : ?x:Integer,Integer
<input>:5:9: syntax: unexpected: implicit name
```
