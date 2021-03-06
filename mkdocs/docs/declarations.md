# Declarations

There is no "top level" in Pinafore.
A program file consists of an expression.
A module consists of an "expose" declaration.

Declarations in `let`-expressions and modules have sequential, not recursive scope.
These are the different kinds of declarations:

- Type declarations
- Subtype declarations
- Import declarations
- Expose declarations
- Bindings
- Recursive blocks

## Recursive Blocks

Recursive blocks are declarations enclosed within `rec ... end`.
Declarations inside a recursive block are declared recursively.
Import declarations, expose declarations, and (nested) recursive blocks are not allowed inside recursive blocks.

```pinafore
let

    rec
    datatype P = MkP (Q -> P);
    datatype Q = MkQ (P -> Maybe Q);
    end;

    rec
    fact = \case
        0 -> 1;
        n -> n * fact (n - 1);
        end;
    end;

in fact 12;
```

## Expose Declarations

Expose declarations provide a simple way of hiding declarations.
Only the specified names will be exposed from the declaration, although subtype relations (which are nameless) will always be exposed.

A module consists of an expose declaration, but they can also be used within `let`-expressions for more fine-grained hiding.

```pinafore
let

    let
    datatype LowerCaseText = MkLowerCaseText Text;
    fromLowerCase: LowerCaseText -> Text;
    fromLowerCase = \(MkLowerCaseText t) -> t;
    toLowerCase: Text -> LowerCaseText;
    toLowerCase t = MkLowerCaseText $ textLowerCase t;
    in expose LowerCaseText, fromLowerCase, toLowerCase; # MkLowerCaseText not exposed

in toLowerCase "Hello";
```

## Import Declarations

An import declaration brings names from a module into scope.

```pinafore
let

import UI; # brings everything from UI into scope

import Colour (AlphaColour, crimson); # brings given names (and subtype relations) from Colour into scope

import Drawing (); # brings only subtype relations from Drawing into scope

in draw {\_ -> source crimson paint}
```
