# Syntax

The syntax of the language is based on Haskell.
These are the main differences:

* Line comments start with `#`, not `--`.
There are no block comments.
* Layout is not significant.
Instead, declarations within a `let` block are separated by `;`.
* There's no "top level" for declarations.
All declarations are local to a `let` block.

A file passed to `pinafore` has syntax `<file>`.
In interactive mode, each line has syntax `<interactive>`.

## Grammar

```no-highlight
<file> ::= <expression>

<interactive> ::= <expression> | <let-declarations> | ":" <interactive-command>

<interactive-command> ::=
    "t" <expression> |
    "type" <expression>

<type> :: =
    <type-1> "|" <type> |
    <type-1> "&" <type> |
    <type-1>

<type-1> ::=
    <type-range-3> "~>" <type-range-3> |
    <type-3> "->" <type-1> |
    <type-2>

<type-2> ::=
    "Order" <type-3> |
    "Ref" <type-range-3> |
    "Set" <type-range-3> |
    <type-3>

<type-3> ::=
    "(" <type> ")" |
    "(" <type> "," <type> ")" |
    "(" ")" |
    <type-var> |
    <type-const>

<type-range-3> ::=
    "{" <type-range-items> "}" |
    <type-range-item>

<type-range-items> ::= | <type-range-items-1>

<type-range-items-1> ::=
    <type-range-item> |
    <type-range-item> "," <type-range-items-1>

<type-range-item> ::=
    <type> |
    "-" <type> |
    "+" <type>

<type-var> ::= symbol -- lowercase first letter

<type-const> ::= symbol -- upper first letter

<expression> ::= <expression-infix[0]>

<expression-infix[n]> ::=
    <expression-infix[n+1]> |
    <expression-infix[n]> <infix-operator[n]> <expression-infix[n+1]>

<expression-infix[10]> ::= <expression-1>

<infix-operator[n]> ::= -- see table

<expression-1> ::=
    "\" <patterns> "->" <expression> |
    <let-declarations> "in" <expression> |
    "if" <expression> "then" <expression> "else" <expression> |
    <expression-2>

<expression-2> ::= <expression-3> | <expression-2> <expression-3>

<expression-3> ::=
    "property" "@"<type-const> "@"<type-const> uuid |
    "point" "@"<type-3> uuid |
    "ref" <expression-3> |
    "%" <expression-3> |
    symbol |
    literal-boolean |
    literal-number |
    literal-text |
    "[" <comma-separated-expressions> "]" |
    "(" <expression> ")" |
    "(" <infix-operator[n]> ")"

<comma-separated-expressions> ::=  | <comma-separated-expressions-1>

<comma-separated-expressions-1> ::=
    <expression> |
    <comma-separated-expressions-1> "," <expression>

<let-declarations> ::= "let" <declarations>

<declarations> ::=  | <declaration> ";" <declarations>

<declaration> ::=
    "entity" <type-const> |
    "subtype" <type-const> "<=" <type-const> |
    symbol <patterns> "=" <expression>

<patterns> ::=  | <pattern> <patterns>

<pattern> ::= symbol
```

## Infix Operators

| [n] | (A x B) x C | A x (B x C) | A x B only |
| --- | --- | --- | --- |
9 | others | `.` `<.>` |
8 | `*` `/` `/\` | |
7 | `+` `-` `\/` | |
6 | | `++` |
5 | | | `==` `/=` `~==` `~/=` `<=` `<` `>=` `>`
4 | | `&&` |
3 | | `||` |
2 | | | `:=` `+=` `-=`
1 | `??` `>>` | |
0 | | `$` |

## Lexical

```no-highlight
symbol = [[:alpha:]][-_[:alnum:]]*

literal-boolean = (true)|(false)

literal-number = (-?[0-9]+(.[0-9]*(_[0-9]*)?)?)|(~-?[0-9]+(.[0-9]*)?(e-?[0-9]+)?)|(NaN)

literal-text = "([^"\\]|\\.)*"

uuid =
    ![[:xdigit:]]{8}-[[:xdigit:]]{4}-[[:xdigit:]]{4}-[[:xdigit:]]{4}-[[:xdigit:]]{12}
```
