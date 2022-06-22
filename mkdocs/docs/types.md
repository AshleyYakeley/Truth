# Types

## Any and None

For all types `T`, `None <: T` and `T <: Any`.

## Data Types

Data types can be declared with the `datatype` keyword.
The declaration specifies the constructors of the type.

Each constructor has a name, and a list of zero or more ambipolar types.

```pinafore
datatype T of
    T1 (Int -> [Int]);
    T2 Int;
end;
```

Data types are not subtypes of `Entity` and are not storable.
You can use closed entity types (below) for that.

Datatypes can take parameters. The variance of each parameter is specified like this:

* `+a` for covariant
* `-a` for contravariant
* `{-p,+q}` or `{+q,-p}` for a contravariant-covariant pair.

For example:

```pinafore
datatype D +a -b {-p,+q} of
    T1 (b -> [a]);
    T2 (p :*: b -> q);
end;
```

## Entity types

Entities are the things that can be represented as subjects and values in the triple-store.
They are of type `Entity`.
Entities include literals, open entity types, and closed entity types.
Closed entity types include lists, maybes, pairs, and eithers of entities, as well as declared closed entity types.

### Literals

`Literal <: Entity`

`Unit <: Literal`

`Boolean <: Literal`

`Ordering <: Literal`

`Number <: Literal`

`Text <: Literal`

`Time <: Literal`

`Duration <: Literal`

`Date <: Literal`

`TimeOfDay <: Literal`

`LocalTime <: Literal`

### Maybe

`Maybe +a`  

 `Maybe Entity <: Entity`.

#### Constructors & Functions
`Just: a -> Maybe a`  
`Nothing: Maybe None`

### Lists

`List +a`  

`List1 +a`  

`List1 a <: List a`

`List Entity <: Entity`.

#### Constructors & Functions
`[]: List None`  
`\x y => x::y: a -> List a -> List1 a`

### Cartesian Products

`+a :*: +b`  

`Entity :*: Entity <: Entity`.

There are no higher-arity tuples than pair.

#### Constructors & Functions
`\x y => (x, y): a -> b -> a :*: b`  
`fst: a :*: Any -> a`  
`snd: Any :*: b -> b`

### Cartesian Sums

`+a :+: +b`  

`Entity :+: Entity <: Entity`.

#### Constructors & Functions
`Left: a -> a :+: None`  
`Right: b -> None :+: b`

### Declared Closed Entity Types

Closed entity types can be declared with the `closedtype` keyword.
The declaration specifies the constructors of the type.
They are similar to data types, but each constructor has an anchor, and field types are all subtypes of `Entity`.
Like data types, closed entity types can have type parameters, but they must all be covariant.

Each constructor has a name, a list of zero or more types (each a subtype of `Entity`), and an anchor.

```pinafore
closedtype Patient of
    LivingPatient Person Date !"Patient.LivingPatient";
    DeadPatient Person Date Date !"Patient.DeadPatient";
end;

patientPerson: Patient -> Person;
patientPerson patient =
    case patient of
        LivingPatient p _ => p;
        DeadPatient p _ _ => p;
    end;
```

Each constructor is anchored by its anchor and its count of types.
Constructors can be added or removed from a closed type without affecting the anchoring of existing constructors in the type.

### Dynamic Entity Types

A dynamic entity is an entity that stores its own "concrete type" (as an anchor).
You can think of the `DynamicEntity` type as a `(concretetype, value)` pair of anchors.

Dynamic entity types are subtypes of `DynamicEntity`, declared by the `dynamictype` keyword.
Each one represents a set of concrete types.
Subtype relationships between them are determined by subset.

A dynamic entity type representing a single concrete type is called a concrete dynamic entity type.
These can be declared like this:

```pinafore
dynamictype Human = !"type.human";
```

You can create dynamic entity types that are the unions of other dynamic entity types, like this:

```pinafore
dynamictype Cat = !"type.cat";
dynamictype Dog = !"type.dog";
dynamictype Animal = Human | Cat | Dog;
```

The [greatest dynamic supertype](dynamic-supertypes.md) of all dynamic entity types is `DynamicEntity`.
So you can use `check`, `coerce`, and pattern-matching to convert between them.

```pinafore
describeAnimalType :: Animal -> Text;
describeAnimalType a = case a of
    h: Human => "Human";
    c: Cat => "Cat";
    d: Dog => "Dog";
    end;
```

To create new values of a concrete dynamic entity type at runtime, you can use `newDynamicEntity`.
Or, you can declare them statically with `dynamicEntity`.

### Open Entity Types

An open entity type is a type to which new entities can be added at run-time.
These types can be declared using `opentype`, and subtype relations between them can be declared using `subtype`:

```pinafore
opentype Animal;
opentype Person;
opentype Cat;
subtype Person <: Animal;
subtype Cat <: Animal;
```

For any open entity type `T`, `T <: Entity`.

#### Subtype Relations

Subtype relations can be declared with `subtype`:

```pinafore
subtype P <: Q;
```

where `Q` is an open entity type, and `P` is a "simple" entity type, that is, a subtype of `Entity` that does not use type parameters.
So `opentype Integer <: Q` is allowed, but `opentype Maybe Integer <: Q` is not (even though `Maybe Integer` is a subtype of `Entity`).

Subtypes relations are transitive.
If there is a loop of subtype relations, it will simply make those types equivalent.

## Functions

`-a -> +b`  

## Actions

`Action +a`  

Roughly equivalent to the Haskell `IO a`.

Actions can _stop_ (using `stop`), which is a kind of exception. Stops can be caught with `onstop`.
Runners of an action that stops, such as the main program, or the handler of a button press, will silently catch the stop.

## Orders

`RefOrder -a`  

An order on a type. Can order by morphisms.

Every order (comparison function) is a `RefOrder`:

`a -> a -> Ordering <: RefOrder a`

## Reference Types

References (of the various reference types) keep track of updates, and will update user interfaces constructed from them when their value changes.

### Whole References

`WholeRef {-p,+q}`

A whole reference a mutable value, that is, something that can be fetched, set, and deleted, either by functions (`get`, `:=`, `delete`), or by a user interface.

Whole references may be "unknown"

### Set References

`SetRef -a`

A set reference is a mutable predicate, like a test on values. Values can be added to it or deleted from it.

### Finite Set References

`FiniteSetRef {-p,+q}`

Finite set references are set references:

`FiniteSetRef -a <: SetRef a`

Finite set references contain a finite number of members, which can be retrieved.

## Morphisms

`{-ap,+aq} ~> {-bp,+bq}`
