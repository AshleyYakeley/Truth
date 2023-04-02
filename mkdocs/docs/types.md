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

Data types are not subtypes of `Entity` and are not storable, except with the `storable` keyword (see below).

Datatypes can take parameters. The variance of each parameter is specified like this:

* `+a` for covariant
* `-a` for contravariant
* `{-p,+q}` or `{+q,-p}` for a contravariant-covariant pair.

For example:

```pinafore
datatype D +a -b {-p,+q} of
    T1 (b -> [a]);
    T2 (p *: b -> q);
end;
```

### Subtypes

It is possible to declare subtypes within a datatype declaration.
For example:

```pinafore
datatype P -x +y of
    MkP (x -> y);
    subtype datatype Q of
        MkQ (x -> Integer) y;
    end;
end;
```

This will give `Q <: P` as well as `D(Q) = D(P)`.

## Storable types

Certain types can be represented as subjects and values in the triple-store.
These types are called _storable_.
All storable types are subtypes of `Entity`.

Storable types include literal types, open entity types, and storable data types.
Storable data types include lists, maybes, products, and sums of storable types, as well as declared storable data types.

### Entity

The `Entity` type represents a 256-bit hash (using the BLAKE3 hashing algorithm).

### Literals

The `Literal` type represents a byte-array in which MIME-type and contents are encoded.
Thus, at run-time ("dynamically") it is possible to enquire into the subtype of a `Literal`, using the greatest-dynamic-supertype mechanism.

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
`fn x, y => x::y: a -> List a -> List1 a`

### Cartesian Products

`+a *: +b`  

`Entity *: Entity <: Entity`.

There are no higher-arity tuples than pair.

#### Constructors & Functions
`fn x, y => (x, y): a -> b -> a *: b`  
`fst: a *: Any -> a`  
`snd: Any *: b -> b`

### Cartesian Sums

`+a +: +b`  

`Entity +: Entity <: Entity`.

#### Constructors & Functions
`Left: a -> a +: None`  
`Right: b -> None +: b`

### Storable Data Types

Storable data types can be declared with the `datatype storable` keywords.
The declaration specifies the constructors of the type.
They are similar to plain data types, but each constructor has an anchor, and field types are all subtypes of `Entity`.
Like plain data types, storable data types can have type parameters, but they must all be covariant.

Each constructor has a name, a list of zero or more types (each a subtype of `Entity`), and an anchor.

```pinafore
datatype storable Patient of
    LivingPatient Person Date !"Patient.LivingPatient";
    DeadPatient Person Date Date !"Patient.DeadPatient";
end;

patientPerson: Patient -> Person
= match
    LivingPatient p _ => p;
    DeadPatient p _ _ => p;
end;
```

Each constructor is anchored by its anchor and its count of types.
Constructors can be added or removed from a storable data type without affecting the anchoring of existing constructors in the type.

Like plain datatypes, storable datatypes permit subtypes in their definitions:

```pinafore
rec
    datatype storable L +x of
        Nil !"L.Nil";
        subtype datatype storable L1 of
            Cons x (L x) !"L.Cons";
        end;
    end;
end;
```

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
describeAnimalType :: Animal -> Text
= match
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

`ModelOrder -a`  

An order on a type. Can order by properties.

Every order (comparison function) is a `ModelOrder`:

`a -> a -> Ordering <: ModelOrder a`

## Model Types

Models (of the various model types) keep track of updates, and will update user interfaces constructed from them when their value changes.

### Whole Models

`WholeModel {-p,+q}`

A whole model a mutable value, that is, something that can be fetched, set, and deleted, either by functions (`get`, `:=`, `delete`), or by a user interface.

Whole models may be "unknown"

### Set Models

`SetModel -a`

A set model is a mutable predicate, like a test on values. Values can be added to it or deleted from it.

### Finite Set Models

`FiniteSetModel {-p,+q}`

Finite set models are set models:

`FiniteSetModel -a <: SetModel a`

Finite set models contain a finite number of members, which can be retrieved.

## Properties

`Property {-ap,+aq} {-bp,+bq}`
