## Any and None

For all types `T`, `None <= T` and `T <= Any`.

## Entity types

Entities are the things that can be represented as subjects and values in the triple-store.

For any open entity type `T`, `NewEntity <= T` and `T <= Entity`.

## Literals

`Literal <= Entity`

`() <= Literal`

`Boolean <= Literal`

`Number <= Literal`

`Text <= Literal`

## Maybe

`Maybe a`  
(`a` is covariant)

`a <= Entity` implies `Maybe a <= Entity`.

#### Constructors & Functions
`Just :: a -> Maybe a`  
`Nothing :: Maybe None`

## Lists

`[a]`  
(`a` is covariant)

`a <= Entity` implies `[a] <= Entity`.

#### Constructors & Functions
`[] :: [None]`  
`\x y -> x:y :: a -> [a] -> [a]`

## Pairs

`(a,b)`  
(both `a` and `b` are covariant)

`a <= Entity` and `b <= Entity` implies `(a,b) <= Entity`.

There are no higher-arity tuples than pair.

#### Constructors & Functions
`\x y -> (x, y) :: a -> b -> (a, b)`  
`fst :: (a, Any) -> a`  
`snd :: (Any, b) -> b`

## Either

`Either a b`  
(both `a` and `b` are covariant)

`a <= Entity` and `b <= Entity` implies `Either a b <= Entity`.

#### Constructors & Functions
`Left :: a -> Either a None`  
`Right :: b -> Either None b`

## Functions

`a -> b`  
(`a` is contravariant, `b` is covariant)

## Actions

`Action`

Roughly equivalent to the Haskell `IO ()`.

## Orders

`Order a`  
(`a` is contravariant)

An order on a type.

## User Interfaces

`UI`

The contents of a user interface window. Can be composed in various ways.

## References

`Ref {-p,+q}`

## Sets

`Set {-p,+q}`

## Morphisms

`{ap,aq} ~> {bp,bq}`
