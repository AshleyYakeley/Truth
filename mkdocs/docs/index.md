# Pinafore

Pinafore is a language that allows you to structure information and create user interfaces for it.

## Installation

You can't, it's not ready yet.

## Invocation

Usage:
```text
pinafore [-i|--interactive] [-n|--no-run] [--data PATH] [SCRIPT]
```

Call `pinafore` with one or more scripts containing actions to run those actions.

By default, pinafore will store local information in the directory `$XDG_DATA_HOME/pinafore` (or else `$HOME/.local/share/pinafore`).
Use `--data` to specify a different directory.

To just parse and type-check a file without running it, use `-n` or `--no-run`.

### Interactive Mode

If `pinafore` is invoked with `-i` or `--interactive`, it will run in interactive mode.
