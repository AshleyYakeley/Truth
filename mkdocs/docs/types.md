## Any and None

For all types T, `None <= T` and `T <= Any`.

## Entity types

Entities are the things that can be represented as points in the triple-store.

For any user-created entity type `T`, `Point <= T` and `T <= Entity`.

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

## Pairs

`(a,b)`, both `a` and `b` are covariant

`a <= Entity` and `b <= Entity` implies `(a,b) <= Entity` (not yet implemented) 

## Either

(not yet implemented)

`Either a b`, both `a` and `b` are covariant

`a <= Entity` and `b <= Entity` implies `Either a b <= Entity` (not yet implemented) 

## References

`Ref {-p,+q}`

## Sets

`Set {-p,+q}`

## Morphisms

`{ap,aq} ~> {bp,bq}`
