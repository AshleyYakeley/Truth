# Dynamic Supertypes

For every subtype realtion. `P <: Q`, there is an implied conversion function of type `P -> Q`.
This conversion does not need to be injective, nor does there need to be "retraction" function of type `Q -> Maybe P`.

But sometimes, this retraction does exist.
Pinafore provides a general mechanism for making use of them.

Every ambipolar type `T` has an ambipolar *greatest dynamic supertype* `D(T)`:

- `D(T)` = `Literal` for all literal types
- `D(T)` = `DynamicEntity` for dynamic entity types (the main use case)
- `D(T)` = `D(S)`, when `T` is a subtype defined in datatype `S`.
- `D(T)` = `T` for all other types

In each case, there is a "check" function that can convert `D(T)` back to `Maybe T`.

## Type Pattern

If `pat` is a pattern of type `T`, then `pat:? T` is a pattern of type `D(T)`. Here's an example:

```pinafore decl
showNumberType: Number -> Text =
match
    i:? Integer => "integer: " <>.Text show i;
    r:? Rational => "rational: " <>.Text show r;
    n => "number: " <>.Text show n;
end
```

## Check

`check` is a [special form](special-forms.md) that provides the retraction function for a given type.

`check @T: D(T) -> Maybe T`

This is equivalent to

`match t:? T => Just t; _ -> Nothing end`

## Coerce

If you're sure that the retraction will always succeed, you can use the `coerce` special form.
(If it doesn't, you'll get a run-time error.)

`coerce @T: D(T) -> T`

This is equivalent to

`match t:?T => t; _ => error "coercion from D(T) to T failed" end`
