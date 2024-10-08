# Declarations & Declarators

There is no "top level" in Pinafore.
A program file consists of an expression.
A module consists of a list of declarations.

## Declarators

Declarators output bindings to be used in the scope of declarations and expressions.

### "Let" Declarators

`let`-declarators output bindings from their contained declarations.
Declarations in `let`-declarators have sequential, not recursive scope.

For example:

```pinafore
let {a=3; b=4} a+b
```

Here `a+b` is an expression, `a=3` and `b=4` are declarations, and `let {a=3; b=4}` is a declarator.
This declarator outputs bindings for `a` and `b` to the scope of the expression `a+b`.

Declarators can also output bindings to declarations:

```pinafore
let {
    let {a=3; b=4} c=a+b;
} c+1
```

Here the declarator `let {a=3; b=4}` outputs bindings for `a` and `b` to the declaration `c=a+b`.

The declarator `let {let {a=3; b=4} c=a+b}` outputs a binding for `c`, but not `a` or `b`, to the expression `c+1`.

A declarator can also be used as a declaration:

```pinafore
let {
    let {a=3; b=4};
    c=a+b;
} c+a+b
```

Here `let {a=3; b=4}` and `c=a+b` are declarations.
The outer declarator outputs bindings for `a`, `b`, and `c` to the expression `c+a+b`.

### Recursive "Let" Declarators

For recursive declarations, use `let rec` to make a recursive "let"-declarator.
Declarator declarations and namespace declarations are not allowed inside recursive blocks.

```pinafore
let {

    let rec {
        datatype P {
            Mk (Q -> P);
        };

        datatype Q {
            Mk (P -> Maybe Q);
        };
    };

    let rec {
        fact = fn {
            0 => 1;
            n => n * fact (n - 1);
        };
    };

 } fact 12
```

### "With" Declarators

"With" declarators copy bindings from a given namespace into the current namespace.
See [Namespaces](#namespaces--namespace-declarations) below.

### "Import" Declarators

"Import" declarators import bindings from a module, which can either be one of the standard libraries, or a module file.

## Declarations
These are the different kinds of declarations:

- Value declarations
- Type declarations
- Subtype declarations
- Standalone declarator declarations
- Declarator-qualified declarations
- Namespace declarations
- Expose declarations

### Value Declarations

Value declarations are of the form `<pattern> = <expression>`.

#### Infix Operators

New operators can be declared with parentheses, like this:

```pinafore
let {
    (&$$&) = fn a,b => a - b;
} 57 &$$& 22
```

The parsing "infixity" of the operator is determined by its name (regardless of namespace) according to [the table](syntax.md#infix-operators),
and is "(A x B) x C" level 10 for other names.

### Type & Subtype Declarations

See [Types](../types/).

### Standalone Declarator Declarations

Any declarator can be used as a declaration.

```pinafore
let {

    let rec {
        x = y;
        y = 4;
    };

    namespace N {
        z = 1;
    };

    with N;

} x + y + z
```

### Declarator-Qualified Declarations

Declarators can qualify declarations just as they can qualify expressions.


```pinafore
let {

    namespace N {
        z = 1;
    };

    with N x = z;

} x
```

### Namespaces & Namespace Declarations

A namespace is a space for declarations.
Each namespace is either the root namespace, or a namespace with a name within another namespace.
This name starts with an upper-case letter.
Thus, each namespace can be identified by a list of zero or more names.

In a given scope, all declaration names are in some namespace.
The full name of a declaration consists of the name and the namespace.
Full names are unique in the scope.

Note that name qualification goes in order specific-to-general, the reverse of most other languages.
So variable `x` inside namespace `N` inside namespace `M` is `x.N.M.`.

#### Referring to Declarations

In a given scope, there is a current namespace.

References to namespaces are either relative or absolute.
Absolute namespace references consist of names separated by dots, with a trailing dot.
These names specify the namespace directly.

Relative namespace references consist of names separated by dots.
These are searched in the current namespace, and then all ancestors of the current namespace.

For example,

* `namespace Metadata.Image.` refers to the namespace `Metadata` within the namespace `Image` within the root namespace.
* `namespace Metadata.Image` refers to the namespace `Metadata` within the namespace `Image` within searched namespaces.

Likewise, references to full names are either relative or absolute.
Absolute full name references consist of names separated by dots, with a trailing dot.
These names specify the namespace directly, and then the name within the namespace.

Relative full name references consist of names separated by dots.
These names are searched in the current namespace, followed by all ancestor namespaces.

For example, if the current namespace is `B.A.`:

* `async.Task.` is always `async.Task.`, that is, it refers to the declaration `async` within the namespace `Task` within the root namespace.
* `async.Task` refers to the first found of these: `async.Task.B.A.`, `async.Task.A.`, `async.Task.`.

#### Current Namespace

All declarations are placed within the current namespace.

A `namespace` declaration specifies the current namespace for the declarations it contains, relative to the existing current namespace.

#### Mapping Namespaces

A `with` declarator aliases names into different namespaces.
For example:

* `with P` maps the contents of namespace `P` into the current namespace.
* `with P (a,b)` maps `a.P` and `b.P` into the current namespace
* `with Q.P` maps the contents of namespace `Q.P` into the current namespace.
* `with P (namespace Q)` maps namespace `Q.P` into the current namespace as `Q`.
* `with P (a,b) as N` maps `a.P` and `b.P` into namespace `N`, so they can be referred to as `a.N` and `b.N`.

#### Example

```pinafore
let {

    namespace A {
        p = 3;

        namespace B {
            q = 4;
        };

    };

    namespace C {
        s = 6;
        with B.A.;
        t = q + 12;
    };

    with A;

    s = p + q.B + s.C;

} s + 1
```

In this example, the scope for `body` contains declarations with these full names, with these values:

```
p.A. = 3
q.B.A. = 4
s.C.A. = 6
t.C.A. = 16
p. = 3
q.B. = 4
s. = 13
```

At the point at which `q.B.A.` is declared, references such as `x` will be searched in this order:

* `x.B.A.` (the current namespace)
* `x.A.` (parent of the above)
* `x.` (parent of the above)

### Documentation Section Declarations

For modules, you can organise the generated documentation of your declarations into sections using `docsec` declarations.

```pinafore decl
#| Some multiples of integers.
docsec "Multiples" {
    double: Integer -> Integer = fn x => x * 2;
    triple: Integer -> Integer = fn x => x * 3;
};
```

You can also add the `docsec` modifier to a namespace declaration: this will create a documentation section with the name of the namespace.

```pinafore decl
namespace docsec A {
    p = 3;
    q = 4;
};
```

Use of `docsec` doesn't affect program semantics, it only changes the output of documentation generation.

### Expose Declarations

Expose declarations provide a simple way of hiding declarations.
Only the specified names will be exposed from the declaration, although subtype relations (which are nameless) will always be exposed.
Expose declarations can be used within `let`-expressions for more fine-grained hiding.

```pinafore
let {

    let {
        # Mk.LowerCaseText not exposed
        datatype LowerCaseText { Mk Text };

        subtype LowerCaseText <: Text = fn Mk.LowerCaseText t => t;

        toLowerCase: Text -> LowerCaseText =
            fn t => Mk.LowerCaseText $ toLowerCase.Text t;
    } expose LowerCaseText, toLowerCase;

    # Outside the above block, there is no way to create a LowerCaseText
    # that is not lower-case text.

} toLowerCase "Hello"
```

It's also possible to expose everything in a namespace:

```pinafore
let {

    let {
        x = 1; # not exposed

        namespace N {
            p = x + 2;
            q = 3;
        };
        
        r = 4;
    } expose namespace N, r;

} p.N + q.N + r
```
