# Modules

You can put declarations in module files to organise your code.
Scripts (and modules) can import modules with the `import` expression.
Like this:

```pinafore
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

```pinafore
let

sometext: Text =
"Hello";

somenumber: Integer =
4;

opentype X;

datatype storable T of
    Mk1 Integer Boolean;
    Mk2;
end;

subtype T <: X;

in expose sometext, somenumber, X, T, T1, T2
```
