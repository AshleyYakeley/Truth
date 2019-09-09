## Any and None

For all types `T`, `None <= T` and `T <= Any`.

## Entity types

Entities are the things that can be represented as subjects and values in the triple-store.
They are of type `Entity`.
Entities include literals, open entity types, and closed entity types.
Closed entity types include lists, maybes, pairs, and eithers of entities, as well as declared closed entity types.

### Literals

`Literal <= Entity`

`() <= Literal`

`Boolean <= Literal`

`Number <= Literal`

`Text <= Literal`

`Time <= Literal`

`Duration <= Literal`

`Day <= Literal`

`TimeOfDay <= Literal`

`LocalTime <= Literal`

### Maybe

`Maybe a`  
(`a` is covariant)

`a <= Entity` implies `Maybe a <= Entity`.

#### Constructors & Functions
`Just :: a -> Maybe a`  
`Nothing :: Maybe None`

### Lists

`[a]`  
(`a` is covariant)

`a <= Entity` implies `[a] <= Entity`.

#### Constructors & Functions
`[] :: [None]`  
`\x y -> x:y :: a -> [a] -> [a]`

### Pairs

`(a,b)`  
(both `a` and `b` are covariant)

`a <= Entity` and `b <= Entity` implies `(a,b) <= Entity`.

There are no higher-arity tuples than pair.

#### Constructors & Functions
`\x y -> (x, y) :: a -> b -> (a, b)`  
`fst :: (a, Any) -> a`  
`snd :: (Any, b) -> b`

### Either

`Either a b`  
(both `a` and `b` are covariant)

`a <= Entity` and `b <= Entity` implies `Either a b <= Entity`.

#### Constructors & Functions
`Left :: a -> Either a None`  
`Right :: b -> Either None b`

### Declared Closed Entity Types

Closed entity types can be declared with the `closedtype` keyword.
The declaration specifies the constructors of the type.

Each constructor has a name, a list of zero or more types (each a subtype of `Entity`), and an anchor.

```pinafore
closedtype Patient =
    LivingPatient Person Date !82572d41-1b36-477e-9252-41610df9d77b |
    DeadPatient Person Date Date !2b678551-2e9d-403a-993e-b61804504809;

patientPerson :: Patient -> Person;
patientPerson patient =
    case patient of
        LivingPatient p _ -> p;
        DeadPatient p _ _ -> p;
    end;
```

Each constructor is anchored by its anchor and its count of types.
Constructors can be added or removed from a closed type without affecting the anchoring of existing constructors in the type.

### Open Entity Types

An open entity type is a type to which new entities can be added at run-time.
These types can be declared using `opentype`, and subtype relations between them can be declared using `subtype`:

```pinafore
opentype Animal;
opentype Person;
opentype Cat;
subtype Person <= Animal;
subtype Cat <= Animal;
```

For any open entity type `T`, `NewEntity <= T` and `T <= Entity`.

Subtypes relations are transitive.
If there is a loop of subtype relations, it will simply make those types equivalent.

## Functions

`a -> b`  
(`a` is contravariant, `b` is covariant)

## Actions

`Action a`  
(`a` is covariant)

Roughly equivalent to the Haskell `IO a`.

Actions can _stop_ (using `stop`), which is a kind of exception. Stops can be caught with `onstop`.
Runners of an action that stops, such as the main program, or the handler of a button press, will silently catch the stop.

## Orders

`Order a`  
(`a` is contravariant)

An order on a type.

## User Interfaces

`UI a`  
(`a` is covariant)

The contents of a user interface window. Can be composed in various ways.
The type parameter is the type of the selection.

## References

`Ref {-p,+q}`

A reference a mutable value, that is, something that can be fetched, set, and deleted, either by functions (`get`, `:=`, `delete`), or by a user interface.

References keep track of updates, and will update user interfaces constructed from them when their value changes.

References may be "unknown"

## Set References

`SetRef a`

(`a` is contravariant)

A set reference is a mutable predicate, like a test on values. Values can be added to it or deleted from it.

## Finite Set References

`FiniteSetRef {-p,+q}`

Finite set references are set references:

`FiniteSetRef -a <= SetRef a`

Finite set references contain a finite number of members, which can be retrieved.

## Morphisms

`{ap,aq} ~> {bp,bq}`
