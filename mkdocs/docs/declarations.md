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

in fact 12
```

## Namespaces

A namespace is a space for declarations.
Each namespace is either the root namespace, or a namespace with a name within another namespace.
This name starts with an upper-case letter.
Thus, each namespace can be identified by a list of zero or more names.

In a given scope, all declaration names are in some namespace.
The full name of a declaration consists of the name and the namespace.
Full names are unique in the scope.

### Referring to declarations

In a given scope, there is a current namespace and a list of search namespaces.

References to namespaces are either relative or absolute.
Absolute namespace references consist of names separated by dots, with a trailing dot.
These names specify the namespace directly.

Relative namespace references consist of names separated by dots.
These names specify the namespace made by concatenating with the current namespace.

For example,

* `namespace Metadata.Image.` refers to the namespace `Metadata` within the namespace `Image` within the root namespace
* `namespace Metadata.Image` refers to the namespace `Metadata` within the namespace `Image` within the current namespace

Likewise, references to full names are either relative or absolute.
Absolute full name references consist of names separated by dots, with a trailing dot.
These names specify the namespace directly, and then the name within the namespace.

Relative full name references consist of names separated by dots.
These names are searched in the following namespaces:

* the current namespace, followed by all ancestor namespaces
* the search namespaces in reverse order of introduction

For example,

* `async.Task.` refers to the declaration `async` within the namespace `Task` within the root namespace
* `async.Task` refers to the first declaration of `async` within the namespace `Task` within the current namespace, followed by all ancestor namespaces, followed by the search namespaces.

### Current namespace

All declarations are placed within the current namespace.

A `namespace` declaration specifies the current namespace for the declarations it contains.

### Search namespaces

A `using` declaration appends the specified namespace to the list of search namespaces.

### Infix operators

Built-in operators are all in the root namespace.
New operators cannot be declared.
No namespace qualification can be given for references to operators.

### Example

```pinafore
let

namespace A of
p = 3;
    namespace B of
    q = 4;
    end;

    namespace B. of
    r = 5;
    end;
end;

namespace C.B.A. of
s = 6;
using B.;
t = r;
end;

using A;

s = p + q.B + s.C.B;

in body
```

In this example, the scope for `body` contains declarations with these full names:

```
p.A.
q.B.A.
r.B.
s.C.B.A.
t.C.B.A.
s.
```

At the point at which `t.C.B.A.` is declared, references such as `r` will be searched in these namespaces in this order:

* `C.B.A.` (the current namespace)
* `B.A.` (parent of the above)
* `A.` (parent of the above)
* root namespace (parent of the above)
* `B.` (search namespace)

## Expose Declarations

Expose declarations provide a simple way of hiding declarations.
Only the specified names will be exposed from the declaration, although subtype relations (which are nameless) will always be exposed.

A module consists of an expose declaration, but they can also be used within `let`-expressions for more fine-grained hiding.

```pinafore
let

    expose LowerCaseText, fromLowerCase, toLowerCase of
        # MkLowerCaseText not exposed
        datatype LowerCaseText of MkLowerCaseText Text end;
        fromLowerCase: LowerCaseText -> Text;
        fromLowerCase = fn MkLowerCaseText t => t;
        toLowerCase: Text -> LowerCaseText;
        toLowerCase t = MkLowerCaseText $ textLowerCase t;
    end;

in toLowerCase "Hello"
```

It's also possible to expose everything in a namespace:

```pinafore
let

    expose namespace N, r of
        x = 1; # not exposed

        namespace N of
            p = x + 2;
            q = 3;
        end;
        
        r = 4;
    end;

in N.p + N.q + r
```

## Import Declarations

An import declaration brings declarations from a module into scope.

```pinafore
let
import "pinafore-media";
in Image.blankImage Colour.honeydew (512,512)
```
