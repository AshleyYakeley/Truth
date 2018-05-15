# Syntax

The syntax of the language is based on Haskell.
These are the main differences:

* Line comments start with `#`, not `--`.
There are no block comments.
* Layout is not significant.
Instead, bindings within a `let` block are separated by `;`.
* There are no tuples as such. Use `[ ]` list syntax instead.

A file passed to `pinafore` has syntax `<file>`.
In interactive mode, each command has syntax `<interactive-command>`.

## Grammar

```
<file> ::= <expression>

<interactive-command> ::= <expression> | <let-bindings>

<expression> ::= <expression-infix[0]>

<expression-infix[n]> ::=
  <expression-infix[n+1]> |
  <expression-infix[n]> <infix-operator[n]> <expression-infix[n+1]>

<expression-infix[10]> ::= <expression-1>

<infix-operator[n]> ::= -- see table

<expression-1> ::=
  "\" <patterns> "->" <expression> |
  <let-bindings> "in" <expression> |
  "if" <expression> "then" <expression> "else" <expression> |
  <expression-2>

<expression-2> ::= <expression-3> | <expression-2> <expression-3>

<expression-3> ::=
  symbol |
  literal-boolean |
  literal-number |
  literal-text |
  "@" |
  literal-predicate |
  literal-point |
  "[" <comma-separated-expressions> "]" |
  "(" <expression> ")"

<comma-separated-expressions> ::=  | <comma-separated-expressions-1>

<comma-separated-expressions-1> ::=
  <expression> |
  <comma-separated-expressions-1> "," <expression>

<let-bindings> ::= "let" <bindings>

<bindings> ::=  | <binding> ";" <bindings>

<binding> ::= symbol <patterns> "=" <expression>

<patterns> ::=  | <pattern> <patterns>

<pattern> ::= symbol
```

## Infix Operators

| Level | (A x B) x C | A x (B x C) | A x B only |
| --- | --- | --- | --- |
9 | others | `.` |
7 | `*` `/` | |
6 | `+` `-` | |
5 | | `++` |
4 | | | `==` `/=` `~==` `~/=` `<=` `<` `>=` `>`
3 | | `&` |
2 | | `|` |
1 | `>>` | |
0 | | `$` |

## Lexical

```no-highlight
symbol = [[:alpha:]][[:alnum:]-_]*

literal-boolean = (true)|(false)

literal-number = (-?[0-9]+(.[0-9]*(_[0-9]*)?)?)|(~-?[0-9]+(.[0-9]*)?(e-?[0-9]+)?)|(NaN)

literal-text = "([^"\\]|\\.)*"

literal-predicate = %[[:xdigit:]]{8}-[[:xdigit:]]{4}-[[:xdigit:]]{4}-[[:xdigit:]]{4}-[[:xdigit:]]{12}

literal-point = ![[:xdigit:]]{8}-[[:xdigit:]]{4}-[[:xdigit:]]{4}-[[:xdigit:]]{4}-[[:xdigit:]]{12}
```
