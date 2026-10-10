# Module Documentation

You can generate CommonMark documentation for a module using `pinadoc`.
A module is anything you can import with an `import` declarator, whether a standard library or your own `.pinafore` file.

For example:

```text
pinadoc UILib
```

For a module at `my/stuff.pinafore` under a local directory, pass the directory with `-I` (or `--include`) and use the module name without the extension:

```text
pinadoc -I path/to/modules my/stuff > stuff.md
```

`pinadoc` uses the same [module search paths](modules.md) as the interpreter.
