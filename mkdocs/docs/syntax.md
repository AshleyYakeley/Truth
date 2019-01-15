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
    "Maybe" <type-3> |
    "Either" <type-3> <type-3> |
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

<type-var> ::= lname

<type-const> ::= uname

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
    "case" <expression> "of" <cases> "end" |
    <expression-2>

<expression-2> ::= <expression-3> | <expression-2> <expression-3>

<expression-3> ::=
    "property" "@"<type-const> "@"<type-const> anchor |
    "entity" "@"<type-3> anchor |
    "{" <expression> "}" |
    "%" <expression-3> |
    lname |
    uname |
    literal-boolean |
    literal-number |
    literal-text |
    "[" <comma-separated(<expression>)> "]" |
    "(" ")" |
    "(" <expression> "," <expression> ")" |
    "(" <expression> ")" |
    "(" <infix-operator[n]> ")"

<comma-separated(n)> ::=  | <comma-separated-1(n)>

<comma-separated-1(n)> ::=
    n |
    <comma-separated-1(n)> "," n

<cases> ::=  | <case> ";" <cases>

<case> ::= <pattern-1> "->" <expression>

<let-declarations> ::= "let" <declarations>

<declarations> ::=  | <declaration> ";" <declarations>

<declaration> ::=
    "opentype" <type-const> |
    "subtype" <type-const> "<=" <type-const> |
    lname <patterns> "=" <expression>

<patterns> ::=  | <pattern-2> <patterns>

<pattern-1> ::= <pattern-2> <patterns>

<pattern-2> ::= <pattern-3> | <pattern-3> ":" <pattern-2>

<pattern-3> ::= <pattern-4> | <pattern-4> "@" <pattern-3>

<pattern-4> ::=
    uname |
    literal-number |
    literal-text |
    lname |
    "_" |
    "[" <comma-separated(<pattern>)> "]" |
    "(" ")" |
    "(" <pattern-1> "," <pattern-1> ")" |
    "(" <pattern-1> ")"
```

## Infix Operators

| [n] | (A x B) x C | A x (B x C) | A x B only |
| --- | --- | --- | --- |
9 | others | `.` `<.>` |
8 | `*` `/` `/\` | `!$` `!$$` `!@` `!@@` |
7 | `+` `-` `\/` `??` | |
6 | | `:` `++` |
5 | | | `==` `/=` `~==` `~/=` `<=` `<` `>=` `>`
4 | | `&&` |
3 | | `||` |
2 | | | `:=` `+=` `-=`
1 | `>>` | |
0 | | `$` |

## Lexical

```no-highlight
uname = [[:upper:]][-_[:alnum:]]*

lname = [_[:lower:]][-_[:alnum:]]*

literal-boolean = (True)|(False)

literal-number = (-?[0-9]+(.[0-9]*(_[0-9]*)?)?)|(~-?[0-9]+(.[0-9]*)?(e-?[0-9]+)?)|(NaN)

literal-text = "([^"\\]|\\.)*"

uuid =
    [[:xdigit:]]{8}-[[:xdigit:]]{4}-[[:xdigit:]]{4}-[[:xdigit:]]{4}-[[:xdigit:]]{12}

anchor = !(uuid|literal-text)
```
