## Application

The application expression `f a` (or `f $ a`) is evaluated in different ways, depending on the types of `f` and `a`.

`f` | `a` | `f a` | Notes
--- | --- | --- | ---
function | *any* | *any* | Ordinary function application.
morphism | entity[*] | entity* | Map the entity along the morphism.
morphism | set[*] | set* | Map a set along the morphism.
inverse morphism | entity[*] | set* | The morphism's preimage set of an entity.
inverse morphism | set[*] | set* | The morphism's preimage set of a set.

## Composition

The composition expression `f . g` is evaluated in different ways, depending on the types of `f` and `g`.

`f` | `g` | `f . g` | Notes
--- | --- | --- | ---
morphism | morphism | morphism | Morphism composition
inverse morphism | inverse morphism | inverse morphism | Inverse morphism composition
*any* | *any* | function | Same as  `\x -> f (g x)`, with applications as above

## Morphism Inversion

The `@` function converts a morphism to an inverse morphism, and vice-versa.
