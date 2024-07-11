# Record Functions

A record function is a function that takes a "record" of parameters for its argument.
The type of each parameter is positive rather than negative and may be polymorphic.
Each parameter may also have a default value.
However, the function itself is monomorphic in the sense that its type cannot share type variables with the types of its parameters.

Here's an example of a record function declaration:

```pinafore
rf of
    p: Text;
    q: Integer = 3;
    m: a -> Maybe a;
end = (m p,m q);
```

You can also specify its type:

```pinafore
rf of
    p: Text;
    q: Integer = 3;
    m: a -> Maybe a;
end : Maybe Text *: Maybe Integer = (m p,m q);
```

And this is how it would be used:

```pinafore
rf of
    p = "text";
    m = Just;
end
```

The `of`...`end` can be omitted, in which case parameters will be taken from the scope. So this is equivalent:

```pinafore
let
    p = "text";
    m = Just;
in rf
```
