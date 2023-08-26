# Type System

The Pinafore type system is based on Stephen Dolan's [Algebraic Subtyping](https://www.cs.tufts.edu/~nr/cs257/archive/stephen-dolan/thesis.pdf), an extension of Hindley-Milner to allow subtyping.
You can see his POPL talk [here](https://www.youtube.com/watch?v=-P1ks4NPIyk).
Pinafore omits record types.

Familiarity with Hindley-Milner is assumed.

## Positive and Negative Types

Algebraic subtyping distinguishes *positive* and *negative* types.
Roughly, a positive type is the type of a value, while a negative type is the type of accepting a value.
Subtyping relations are of the form `A <: B`, where `A` is a positive type and `B` is a negative type.
You can read `A <: B` to mean "every A is a B", or that something of type `A` will be accepted as a `B`.
This relation is of course reflexive and transitive.

For example, if `P -> Q` is a positive type, then `P` is a negative type, and `Q` is a positive type.
Given a function `f: P -> Q` and a value `a: T`, then the application `f a: Q` is allowed if `T <: P`.

(Alternatively, if `P -> Q` is a negative type, then `P` is a positive type, and `Q` is a negative type.)

## Covariance and Contravariance

More generally, every type parameter of a type is either covariant or contravariant.
Covariant parameters have the same polarity (positive or negative) as the type they are in, while contravariant parameters have the opposite.

If `T` has a covariant parameter,

`A <: B` implies `T A <: T B`

If `T` has a contravariant parameter,

`A <: B` implies `T B <: T A`

Intuitively, the word "of" suggests covariance, while the words "for" and "on" suggest contravariance.
For example, a number is a literal (`Number <: Literal`), and a list *of* numbers is a list of literals (`[Number] <: [Literal]`).
Thus "list of" is covariant.
And an order *on* literals is an order on numbers (`ModelOrder Literal <: ModelOrder Number`).
Thus "order on" is contravariant.

## Any & None

`Any` and `None` are the top and bottom of the type hierachy. That is, for any `P`, we have `None <: P` and `P <: Any`.

`Any` is only a negative type, and `None` is only a positive type.

## Conjunction and Disjunction

If `P` and `Q` are positive types, then `P | Q` is a positive type.
This represents "untagged union", that is, the type of values that may be `P` or may be `Q`, but there is no way of determining which.

If `P` and `Q` are negative types, then `P & Q` is a negative type.
A value passed to a `P & Q` must be both a `P` and a `Q`.

## Type Variables

As in Haskell, the first letter is upper case for type constants, and lower case for type variables.

Type variables play essentially the same role as they do in Hindley-Milner typing, though in the context of Algebraic subtyping they are best understood as connecting (negative) inputs to (positive) outputs.
Type variables on only one side can be eliminated.

## Recursive types

Equirecursive types are not much used in Pinafore, however, they are necesssary as principal types for certain expressions.

If `a` is a type variable, and `F a` is a type with only covariant use of `a`, then `rec a, F a` is a type with the same polarity as `F a`.

The essential fact of recursive types is that `rec a, F a` and `F (rec a, F a)` are equivalent.

## Type Simplification

1. Unused recursion is eliminated.  
`rec a, (a | T)` &rarr; `rec a, T`  
`rec a, a` &rarr; `Any` or `None`  
`rec a, T` &rarr; `T` (if `a` does not appear in `T`)

1. Any type variables that are "fully constrained" are eliminated (i.e., replaced with `None` or `Any`).  
`(a & Text) -> a | Literal` &rarr; `Text -> Literal` (because `Text <: Literal`)  
This implies eliminating variables that appear only in the positive position, or only in the negative position.  
`fn x, y => x: a -> b -> a` &rarr; `fn x, y => x: a -> Any -> a`  
`[]: [a]` &rarr; `[]: [None]`

1. `None` and `Any` act as identities for `|` and `&`, respectively.  
`Int | None` &rarr; `Int`

1. Redundant types in joins (`|` or `&`) are eliminated.  
`Text & Text` &rarr; `Text`.  
More generally, if `P <: Q` then  
`P | Q` &rarr; `Q`  
`P & Q` &rarr; `P`

1. Matching parameterised types are collapsed along their parameters. For example:  
`(A -> X) | (B -> Y)` &rarr; `(A & B) -> (X | Y)`  
`[A] & [B]` &rarr; `[A & B]`

1. Type variables are merged if they appear in all the same positive positions, or in all the same negative positions.  
`a -> b -> (a | b)` &rarr; `a -> a -> a` (`a` and `b` appear in the same set of positive positions)

1. Recursive types are rolled up.  
`F (rec a, F a)` &rarr; `rec a, F a`

## Type Ranges

To represent a parameterised type with a parameter that would not ordinarily be either covariant or contravariant, a *type range* is used.
This is simply a pair of type parameters, one contravariant (indicated with a minus sign) and one covariant (a plus sign).
Type ranges in types are typically represented by the form `{-p,+q}`, where `p` is the contravariant parameter and `q` the covariant.
However, the syntax permits any number of comma-separated items. For example, in positive position:

`T {}` = `T {-Any,+None}`  
`T {+a}` = `T {-Any,+a}`  
`T {+Int,-a,-Entity}` = `T {-(a & Entity),+Int}`

In negative position,

`T {}` = `T {-None,+Any}`  
`T {+a}` = `T {-None,+a}`  
`T {+Int,-a,-Entity}` = `T {-(a | Entity),+Int}`

And also,

`T A` = `T {A}` = `T {-A,+A}`  
`T +A` = `T {+A}`  
`T -A` = `T {-A}`  

It's important to remember that `-` and `+` indicate contravariance and covariance, not negative and positive polarity.

One consequence of this that may trip you up is that, for example, `T +A` in positive and negative position are not the same.
Thus a type such as `T +A -> T +A` may not include the identity function, since this type is actually equivalent to `T {-None,+A} -> T {-Any,+A}`.

## Type Inversion

Type inversion is converting a positive type to a negative type, or vice-versa.
Specifically, given a positive type `T`, there is some set `S(T)` of positive types that can subsume to `T`.
The inverse of `T` is a negative type `T'` such that the set of positive types that unify with `T'`, is the same as `S(T)`.

As a rule of thumb, types that, after simplification, have free type variables, or mention `|`, `&`, `Any`, or `None`, cannot be inverted.

Type inversion shows up in type signatures inside function bindings. Here's an example, given some positive type `T`:

`f = fn x => let y:T = x in y`

If `T` can be inverted (to `T'`), the type of `f` is `T' -> T`. Otherwise Pinafore will reject the expression with a "cannot invert type" error.
