# Invocation

Usage:
```text
pinafore [--data PATH] [-I|--include PATH] [-n|--no-run] [--imply name=value] SCRIPTPATH [ARGUMENT...]
```

Call `pinafore` with a script containing actions to run those actions.
The script must consist of an expression of a subtype of `Action Any`.

To just parse and type-check a file without running it, use `-n` or `--no-run`.

You can pass in implicit variables (of type `Text`) with `--imply`,
e.g. `--imply myvar=xyz` is equivalent to `imply ?myvar: Text = "xyz" in ...`.

Use `--data` to set the data directory (this also sets `$PINAFOREDIR` within the script).
If not specified, the data directory will be `$PINAFOREDIR`, or else `$XDG_DATA_HOME/pinafore`, or else `$HOME/.local/share/pinafore`.
The data directory is used by `openLocal.Store` for the directory to put local storage.

Use `--include` or `-I` to set directories to search for scripts for `import`.
The directory `lib` within the data directory will also be searched.

If you want to make a script executable from the command line, you can put this at the top, in the usual Unix fashion:

```text
#!/usr/bin/env pinafore1
```

or (expecially on NixOS)

```text
#!/usr/bin/env pinafore1
```

## Interactive Mode

```text
pinafore [--data PATH] [-I|--include PATH] (-i|--interactive)
```

If `pinafore` is invoked with `-i` or `--interactive`, it will run in interactive mode.
This may be particularly helpful for understanding the type system.

At the prompt, you can enter:

* An expression. If this is an action, it will be executed, otherwise its value will be printed.
* A let-expression (or other declarator), which will add bindings to the context.
* A special command:
    * ":doc name" will show the documentation for a name.
    * ":type expression" will show the inferred type of an expression.
    * ":simplify+ type" will show a simplified positive type.
    * ":simplify- type" will show a simplified negative type.

See the `<interactive>` element in the [grammar](syntax.md#grammar).

Here's an example:

```text
pinafore> 3 + 4
7
pinafore> p = [5,6,7] <> [1,1,1]
pinafore> f = fn x => x <> x
pinafore> :type f
: List a -> List a
pinafore> f p
[5,6,7,1,1,1,5,6,7,1,1,1]
pinafore> :simplify+ (a -> Literal) | ((Text & b) -> a)
Text -> Literal
```
