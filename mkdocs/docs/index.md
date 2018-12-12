# Pinafore

Pinafore consists of a language that allows you to create user interfaces to information stored locally and elsewhere.

## Installation

## Invocation

Usage: `pinafore ([-i|--interactive] [-n|--no-run] [--data PATH] [SCRIPT] | [--doc] | [--dump-table] [--data PATH])`

Call `pinafore` with one or more scripts containing actions to run those actions.

By default, pinafore will store local information in the directory `$XDG_DATA_HOME/pinafore`.
Use `--data` to specify a different directory.

To just parse and type-check a file without running it, use `-n` or `--no-run`.

### Interactive Mode

If `pinafore` is invoked without any scripts and standard input is a terminal, then it will run in interactive mode.
Interactive mode can also be forced using `-i` or `--interactive`.
