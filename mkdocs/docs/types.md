## Any and None

For all types T, `None <= T` and `T <= Any`.

## Entity types

Entities are the things that can be represented as subjects and values in the triple-store.

For any open entity type `T`, `NewEntity <= T` and `T <= Entity`.

## Literals

`Literal <= Entity`

`() <= Literal`

`Boolean <= Literal`

`Number <= Literal`

`Text <= Literal`

## Functions

`a -> b`, a is contravariant, b is covariant

## Lists

`[a]`, a is covariant

## Actions

`Action`, roughly equivalent to the Haskell `IO ()`

## Orders

`Order a`, a is contravariant

An order on a type.

## User Interfaces

`UI`

The contents of a user interface window. Can be composed in various ways.

## Maybe

`Maybe a`, a is covariant

`a <= Entity` implies `Maybe a <= Entity`

## Pairs

`(a,b)`, both `a` and `b` are covariant

`a <= Entity` and `b <= Entity` implies `(a,b) <= Entity`

## Either

`Either a b`, both `a` and `b` are covariant

`a <= Entity` and `b <= Entity` implies `Either a b <= Entity`

## References

`Ref {-p,+q}`

## Sets

`Set {-p,+q}`

## Morphisms

`{ap,aq} ~> {bp,bq}`
