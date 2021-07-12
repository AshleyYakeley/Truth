# Modules

You can put declarations in module files to organise your code.
Scripts (and modules) can import modules with the `import` declaration.
Like this:

```pinafore
let
import My.Stuff;
in outputLn sometext
```

Module names are one or more unames, separated by periods.

You can also qualify names (of types, constructors, values) with module names, which will import the module and find the name in it:

```pinafore
outputLn My.Stuff.sometext
```

To import the module `My.Stuff`, Pinafore will look for a file in these paths in this order:

1. `$dir/My/Stuff.pinafore` for each `-I $dir` on the command line
2. `$pinafore/lib/My/Stuff.pinafore`, where `$pinafore` is the local pinafore directory (typically `$HOME/.local/share/pinafore`)
3. `/usr/local/share/pinafore/lib/My/Stuff.pinafore`
4. `/usr/share/pinafore/lib/My/Stuff.pinafore`

A module file looks something like this (see [syntax](syntax.md)):

```pinafore
let
sometext :: Text;
sometext = "Hello";

somenumber :: Integer;
somenumber = 4;

opentype X;

closedtype T = T1 Integer Boolean | T2;

subtype T <: X;

in expose sometext somenumber X T T1 T2
```

The `expose` statement exposes the given names (values, types, type constructors) that will be available when imported.
Subtype relations are always exposed.
