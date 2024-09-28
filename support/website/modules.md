# Modules

You can put declarations in module files to organise your code.
Scripts (and modules) can import modules with the `import` expression.
Like this:

```pinafore nocheck
import "my/stuff" in
outputLn.Env sometext
```

To import the module `my/stuff`, Pinafore will look for a file in these paths in this order:

1. `$dir/my/stuff.pinafore` for each `-I $dir` on the command line
2. `$pinafore/lib/my/stuff.pinafore`, where `$pinafore` is the local pinafore directory (typically `$HOME/.local/share/pinafore`)
3. `/usr/local/share/pinafore/lib/my/stuff.pinafore`
4. `/usr/share/pinafore/lib/my/stuff.pinafore`

A module file is a list of declarations, though very often it is a single `expose` declaration,
something like this (see [syntax](syntax.md)):

```pinafore decl
let {

sometext: Text =
    "Hello";

somenumber: Integer =
    4;

entitytype X;

datatype storable T {
    Mk1 Integer Boolean !"Mk1.P";
    Mk2 !"Mk2.P";
};

subtype T <: X;

} expose sometext, somenumber, X, T, Mk1.T, Mk2.T;
```
