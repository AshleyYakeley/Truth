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
        datatype P of
        MkP (Q -> P);
        end;

        datatype Q of
        MkQ (P -> Maybe Q);
        end;
    end;

    rec
    fact = match
        0 => 1;
        n => n * fact (n - 1);
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
    datatype LowerCaseText of MkLowerCaseText Text end;
    fromLowerCase: LowerCaseText -> Text;
    fromLowerCase = fn MkLowerCaseText t => t;
    toLowerCase: Text -> LowerCaseText;
    toLowerCase t = MkLowerCaseText $ textLowerCase t;
    in expose LowerCaseText, fromLowerCase, toLowerCase; # MkLowerCaseText not exposed

in toLowerCase "Hello";
```

## Import Declarations WRONG

An import declaration brings names from a module into scope.

```pinafore
let

import "pinafore-gnome"; # brings everything from GTK into scope

import "pinafore-media" (AlphaColour, crimson); # brings given names (and subtype relations) from Colour into scope

import "pinafore-media" (); # brings only subtype relations from Cairo into scope

in draw {fn _ => source crimson paint}
```
