# Types

## Any and None

For all types `T`, `None <: T` and `T <: Any`.

## Data Types

Data types can be declared with the `datatype` keyword.
The declaration specifies the constructors of the type.
Each constructor has a name, which starts with a capital letter.
These names are in the namespace created by the name of the type.

There are two kinds of constructor, _plain_ and _record_.
A datatype declaration can mix plain and record constructors.

Data types are not subtypes of `Entity` and are not storable, except with the `storable` keyword (see below).

### Plain Constructors

A plain constructor consists of its name, and a list of zero or more ambipolar types, with no free type variables besides parameters.

Here's an example of a type with two constructors, `Mk1.T` and `Mk2.T`:

```pinafore decl
datatype T {
    Mk1 (Integer -> List Integer);
    Mk2 Integer Text;
};
```
### Record Constructors

A record constructor consists of its name, and a list of zero or more _members_ in an `{`...`}` block.

A member consists of a name and a positive type (which may have free type variables), and optionally a "default" value.

Here's an example of a type with a record constructor with three members:

```pinafore decl-cont
datatype T {
    Mk {
        f: List a -> Maybe a;
        g: Text -> Action Unit;
        h: Integer *: Integer = (100, 50);
    };
};
```

As values, record constructors behave the same way as [record functions](record-functions.md).
To create a value of the type `T` using its record constructor `Mk.T`, set the member values inside `{` ... `}`, like this:

```pinafore decl-cont
t: T =
Mk.T {
    f = fn {[] => Nothing; a :: _ => Just a;};
    g = outputLn.Env.;
    h = (52,1);
};
```

Alternatively, you can omit `{` ... `}` and instead bring values matching its members into scope, like this:

```pinafore decl-cont
t: T =
let {
    f = fn {[] => Nothing; a :: _ => Just a;};
    g = outputLn.Env.;
    h = (52,1);
} Mk.T;
```

If a member has a default value, then you can omit it:

```pinafore decl-cont
t: T =
Mk.T {
    f = fn {[] => Nothing; a :: _ => Just a;};
    g = outputLn.Env.;
};
```

Values can be retrieved by matching the constructor, like this:


```pinafore decl
tg: T -> Text -> Action Unit =
    fn Mk.T => g;
```

### Record Inheritance

A datatype `T` can inherit from one or more supertypes `S1`, `S2`, etc. provided that:

* Supertypes `S1`, `S2`, etc. are datatypes without type parameters.
* All contructors of `T` are record constructors.
* Every record constructor of `T` mentions a record constructor from each supertype `S1`, `S2`, etc.

This will give `T <: S1`, `T <: S2`, etc.
However, note that unlike classes in languages such as Java, these conversions are not injective,
the [greatest dynamic supertype](dynamic-supertypes.md) of `T` is still `T`, and no downcasting is possible.

Here's an example:

```pinafore decl
datatype S1 {
    Mk {
        p: Rational;
        q: Text -> Text;
    };
};

datatype S2 {
    Mk {
        r: Action Unit;
    };
};

datatype T <: S1 & S2 {
    Mk {
        Mk.S1;
        Mk.S2;
        x: Number;
    };
};
```

All members of each mentioned supertype constructor will be in the defined constructor.
In this case, the constructor `Mk.T` has members `p`, `q`, `r`, `x`.

#### Member Refinement

It is possible to refine the type of a constructor member,
with a type that subsumes to previous type.
For example:


```pinafore decl
datatype S {
    Mk {
        n: Rational;
        f: Text -> Text;
    };
};

datatype T <: S {
    Mk {
        Mk.S;
        n: Integer;
        f: a -> a;
    };
};
```

In this case, constructor `Mk.T` has two members, `n` and `f`,
of types that subsume to the corresponding types in `Mk.S`.

If a same member name is defined in constructors from two different supertypes,
the member must be defined in the type as well.

#### Multiple Inheritance Diamonds

Multiple inheritance "diamonds" are permitted, provided they are consistent.

For example, this is consistent:


```pinafore decl
datatype A {
    Mk1 {
    };
    Mk2 {
    };
};

datatype B <: A {
    Mk1 {
        Mk1.A;
    };
    Mk2 {
        Mk2.A;
    };
};

datatype C <: A {
    Mk1 {
        Mk1.A;
    };
    Mk2 {
        Mk2.A;
    };
};

datatype D <: B & C {
    Mk {
        Mk1.B;
        Mk1.C;
    };
};
```

However, this would be inconsistent:


```pinafore nocheck
datatype D <: B & C {
    Mk {
        Mk1.B;
        Mk2.C;
    };
};
```

This is because the conversions `D <: B <: A` and `D <: C <: A` would be different.

### Parameters

Datatypes can take parameters. The variance of each parameter is specified like this:

* `+a` for covariant
* `-a` for contravariant
* `(-p,+q)` or `(+q,-p)` for a contravariant-covariant pair
* `a` for a contravariant-covariant pair (known as a _double_ parameter).

For example:

```pinafore decl
datatype D +a -b (-p,+q) {
    Mk1 (b -> List a);
    Mk2 (p *: b -> q);
};
```

A double parameter is really a contravariant-covariant pair of parameters, that makes working with pairs easier.
For example:

```pinafore decl
datatype D a {
    Mk (a -> a);
};
```

This is equivalent to:

```pinafore decl
datatype D (-p,+q) {
    Mk (p -> q);
};
```

### Subtypes

It is possible to declare subtypes within a datatype declaration.
For example:

```pinafore decl
datatype P -x +y {
    Mk (x -> y);
    subtype datatype Q {
        Mk (x -> Integer) y;
    };
};
```

This creates types `P` and `Q.P`, as well as constructors `Mk.P` and `Mk.Q.P`.

This will give `Q.P <: P` as well as `D(Q.P) = D(P)`.

## Storable types

Certain types can be represented as subjects and values in the triple-store.
These types are called _storable_.
All storable types are subtypes of `Entity`.

Storable types include literal types, open entity types, and storable data types.
Storable data types include lists, maybes, products, and sums of storable types, as well as declared storable data types.

### Entity

The `Entity` type represents a 256-bit hash (using the BLAKE3 hashing algorithm).

### Literals

The `Literal` type represents a byte-array in which type and contents are encoded.
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

```pinafore decl
entitytype Person;

datatype storable Patient {
    Living Person Date !"Patient.Living";
    Dead Person Date Date !"Patient.Dead";
};

patientPerson: Patient -> Person =
fn {
    Living.Patient p _ => p;
    Dead.Patient p _ _ => p;
};
```

Each constructor is anchored by its anchor and its count of types.
Constructors can be added or removed from a storable data type without affecting the anchoring of existing constructors in the type.

Like plain datatypes, storable datatypes permit subtypes in their definitions:

```pinafore decl
let rec {
    datatype storable L +x {
        Nil !"L.Nil";
        subtype datatype storable L1 {
            Cons x (L x) !"L.Cons";
        };
    };
};
```

### Open Entity Types

An open entity type is a type to which new entities can be added at run-time.
These types can be declared using `entitytype`, and subtype relations between them can be declared using `subtype`:

```pinafore decl
entitytype Animal;
entitytype Person;
entitytype Cat;
subtype Person <: Animal;
subtype Cat <: Animal;
```

Every open entity type is a subtype of `Entity`, and their representations are the same.

#### Subtype Relations

Subtype relations can be declared with `subtype`:

```pinafore nocheck
subtype P <: Q;
```

where `Q` is an open entity type, and `P` is a "simple" entity type, that is, a subtype of `Entity` that does not use type parameters.
So `entitytype Integer <: Q` is allowed, but `entitytype Maybe Integer <: Q` is not (even though `Maybe Integer` is a subtype of `Entity`).

Subtypes relations are transitive.
If there is a loop of subtype relations, it will simply make those types equivalent.

## Predicate Types

A predicate type `T` is a "subset" of an existing type `P`, consisting of those values that satisfy some predicate `f: P -> Boolean`.
You can declare them like this:

```pinafore nocheck
predicatetype T <: P = f;
```

The parent type `P` must be an invertible type (that is, with no type variables, `&`, `|`, `Any` or `None`).
This will create a new type `T`, with subtype relation `T <: P` and [greatest dynamic supertype](dynamic-supertypes.md) `D(T) = D(P)`.

To create new values of `T`, you can use `!{check @T}` and `!{coerce @T}`.

For example, a type of even integers:
```pinafore decl
predicatetype Even <: Integer = fn i => mod i 2 == 0;

addEven: Even -> Even -> Even = fn a, b => !{coerce @Even} $ a + b;

even4: Even = !{coerce @Even} 4;
```

The greatest dynamic supertype of `Even` will be `Literal` (since that is the GDS of `Integer`).

You can also create storable predicate types from storable parent types:
```pinafore decl
predicatetype storable Even <: Integer = fn i => mod i 2 == 0;
```

## Functions

`-a -> +b`  

As in Haskell, functions are pure.

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

`WholeModel (-p,+q)`

A whole model a mutable value, that is, something that can be fetched, set, and deleted, either by functions (`get`, `:=`, `delete`), or by a user interface.

Whole models may be "unknown", indicating a missing value. Fetching the value with `get` will cause a stop.

### Set Models

`SetModel -a`

A set model is a mutable predicate, like a test on values. Values can be added to it or deleted from it.

### Finite Set Models

`FiniteSetModel (-p,+q)`

Finite set models are set models:

`FiniteSetModel -a <: SetModel a`

Finite set models contain a finite number of members, which can be retrieved.

## Properties

`Property (-ap,+aq) (-bp,+bq)`
