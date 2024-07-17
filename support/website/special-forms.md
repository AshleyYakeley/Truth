# Special Forms

Special forms are values in libraries that take types and/or anchors as "arguments".
The type of a special form typically depends on the type arguments passed in with `@`, if required.
For example:

```pinafore nocheck
check @A: D(A) -> Maybe A;
point.OpenEntity @A <anchor>: A;
```

Here `@A` indicates a type argument, and `D(A)` indicates the [greatest dynamic supertype](dynamic-supertypes.md) of `A`.
`<anchor>` indicates an anchor argument.

So, `check @Integer` of type `Literal -> Maybe Integer` determines whether a given `Literal` is an `Integer`.
And `point.OpenEntity @MyEntity !"somepoint"` is a point entity for the anchor `!"somepoint"` of type `Entity`.

You cannot define new special forms in Pinafore, they can only be defined in Haskell libraries.
