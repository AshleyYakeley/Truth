# Modules

You can put declarations in module files to organise your code.
Scripts (and modules) can import modules with the `import` declaration.
Like this:

```pinafore
let
import "my/stuff";
in Env.outputLn sometext
```

To import the module `my/stuff`, Pinafore will look for a file in these paths in this order:

1. `$dir/my/stuff.pinafore` for each `-I $dir` on the command line
2. `$pinafore/lib/my/stuff.pinafore`, where `$pinafore` is the local pinafore directory (typically `$HOME/.local/share/pinafore`)
3. `/usr/local/share/pinafore/lib/my/stuff.pinafore`
4. `/usr/share/pinafore/lib/my/stuff.pinafore`

A module file looks something like this (see [syntax](syntax.md)):

```pinafore
expose sometext, somenumber, X, T, T1, T2 of
sometext: Text
= "Hello";

somenumber: Integer
= 4;

opentype X;

datatype storable T of
    T1 Integer Boolean;
    T2;
end;

subtype T <: X;
end
```

The `expose` statement exposes the given names (values, types, type constructors) that will be available when imported.
Subtype relations are always exposed.
