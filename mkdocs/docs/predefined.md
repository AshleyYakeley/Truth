Entries in italics are supertypes of existing types, for convenience.

## Special Forms

These are built-in keywords that resemble predefined bindings.

**property @A @B <anchor>** : `A ~> B`  
A property for this anchor. `A` and `B` are types that are subtypes of `Entity`.

**entity @A <anchor>** : `A`  
An open entity for this anchor. `A` is an open entity type.

**evaluate @A** : `Text -> Action (Either Text A)`  
A function that evaluates text as a Pinafore expression to be subsumed to positive type `A`.
The result of the action is either the value (`Right`), or an error message (`Left`).
The local scope is not in any way transmitted to the evaluation.

{!predefined.md!}
