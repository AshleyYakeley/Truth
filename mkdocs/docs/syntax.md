# Syntax

The syntax of the language is based on Haskell.
These are the main differences:

* Line comments start with `#`, not `--`.
* Block comments start with `{#` and end with `#}` (and may be nested).
* Layout is not significant.
Instead, declarations within a `let` block, lines within a `do` statement, and cases within a `case` statement, are separated by `;`.
Also, `case` and `do` statements are terminated with `end`.
* `:` is used for type signatures, while `::` is used for list construction.
* There's no "top level" for declarations.
All declarations, including type declarations, are local to a `let` block.
* Only one equation is allowed for a function definition. Use `case` to match argument patterns.
* Haskell's type constructors `Either`, `(,)`, `[]` and `()` become `:+:`, `:*:`, `List` and `Unit`.
* There's no tuple type bigger than two. The tuple `(a,b,c)` is equivalent to `(a,(b,c))`, etc.
* Lambda-expressions and case-expressions use `=>` instead of `->`.

## Grammar

* A script file passed to `pinafore` has syntax `<script>`.
* Modules loaded with `import` have syntax `<module>`.
* In interactive mode, each line has syntax `<interactive>`.

```text
<script> ::= <expression>

<module> ::= <expose-declaration>

<names> ::= <comma-separated(<name>)>

<name> ::= uname | lname | "(" <infix-operator[n]> ")"

<refname> ::= uname | lname | "(" <infix-operator[n]> ")"

<qname> ::= quname | qlname

<interactive> ::= <expression> | <let-declarations> | ":" <interactive-command>

<interactive-command> ::=
    "doc" <qname> |
    "type" <expression> |
    "simplify" "+" <type> |
    "simplify" "-" <type>

<type> :: =
    "rec" <type-var> "." <type>
    <type-0>

<type-0> :: =
    <type-1> "|" <type> |
    <type-1> "&" <type> |
    <type-1>

<type-infix> ::= "->" | "~>"

<type-1> ::= <type-infix[0]> |

<type-infix[n]> ::=
    <type-infix[n+1]> |
    <type-infix[n]> <type-infix-operator[n,left]> <type-infix[n+1]> |
    <type-infix[n+1]> <type-infix-operator[n,right]> <type-infix[n]>

<type-infix[4]> ::= <type-2>

<type-infix-operator[n,dir]> ::= -- see table

<type-2> ::=
    <type-const> <type-arguments> |
    <type-3>

<type-arguments> ::=
    <type-argument> <type-arguments> |
    <type-argument>

<type-argument> ::=
    <type-3> |
    "{" <type-range-items> "}" |
    "-" <type-3> |
    "+" <type-3>

<type-3> ::=
    "(" <type> ")" |
    <type-var> |
    <type-const>

<type-range-items> ::= | <type-range-items-1>

<type-range-items-1> ::=
    <type-range-item> |
    <type-range-item> "," <type-range-items-1>

<type-range-item> ::=
    <type> |
    "-" <type> |
    "+" <type>

<type-var> ::= lname

<type-const> ::= quname

<expression> ::=
    <expression-infix[0]> |
    <expression> ":" <type>

<expression-infix[n]> ::=
    <expression-infix[n+1]> |
    <expression-infix[n]> <infix-operator[n,left]> <expression-infix[n+1]> |
    <expression-infix[n+1]> <infix-operator[n,right]> <expression-infix[n]>

<expression-infix[11]> ::= <expression-1>

<infix-operator[n,dir]> ::= -- see table

<expression-1> ::=
    "\" <patterns> "=>" <expression> |
    <let-declarations> "in" <expression> |
    "if" <expression> "then" <expression> "else" <expression> |
    "case" <expression> "of" <cases> "end" |
    "\" "case" <cases> "end" |
    "do" <do-lines> <expression> "end" |
    <expression-2>

<expression-2> ::= <expression-3> | <expression-2> <expression-3>

<annotation> ::= "@" <type-3> | anchor

<annotations> ::= <annotation> | <annotations> <annotation>

<expression-3> ::=
    "{" <expression> "}" |
    "%" <expression-3> |
    <expression-specialform> |
    <expression-var> |
    <constructor> |
    <literal> |
    "[" <comma-separated(<expression>)> "]" |
    "(" ")" |
    "(" <expression> "," <comma-separated-1(<expression>)> ")" |
    "(" <expression> ")" |
    "(" <infix-operator[n]> ")"

<expression-var> ::= qlname

<expression-specialform> ::= qlname <annotations>

<comma-separated(n)> ::=  | <comma-separated-1(n)>

<comma-separated-1(n)> ::=
    n |
    <comma-separated-1(n)> "," n

<cases> ::=  | <case> ";" <cases>

<case> ::= <pattern-1> "=>" <expression>

<do-lines> =  | <do-line> ";" <do-lines>

<do-line> = <expression> | <pattern-1> "<-" <expression>

<let-declarations> ::= "let" <declarations>

<declarations> ::=  | <declaration> ";" <declarations>

<declaration> ::=
    direct-declaration |
    "import" <module-name> <opt-import-list> |
    <expose-declaration> |
    "rec" <direct-declarations> "end"

<opt-import-list> ::=  | "(" <names> ")"

<direct-declarations> ::=  | <direct-declaration> ";" <direct-declarations>

<direct-declaration> ::=
    "datatype" <type-const> <datatype-parameters> "of" <datatype-body> "end" |
    "opentype" <type-const> |
    "subtype" <type-const> "<:" <type-const> |
    "closedtype" <type-const> <closedtype-parameters> "of" <closedtype-body> "end" |
    "dynamictype" <type-const> "=" <dynamictype-constructors> |
    <binding>

<expose-declaration> ::= <let-declarations> "in" <expose-declaration> |
    "expose" <names>

<module-name> ::= uname | uname "." <module-name>

<binding> ::=
    <type-signature> ";" <unsigned-binding> |
    <unsigned-binding>

<unsigned-binding> ::= lname <patterns> "=" <expression>

<type-signature> ::= lname ":" <type>

<datatype-parameters> ::=  | <datatype-parameter> <datatype-parameters>

<datatype-parameter> ::=
    "+" lname |
    "-" lname |
    "{" "+" lname "," "-" lname "}" |
    "{" "-" lname "," "+" lname "}" |

<datatype-body> ::=  | <datatype-constructors>

<datatype-constructors> ::=
    <datatype-constructor> |
    <datatype-constructor> ";" <datatype-constructors>

<datatype-constructor> ::=
    uname <types> |
    "subtype" "datatype" <type-const> "of" <datatype-body> "end"

<closedtype-parameters> ::=  | <closedtype-parameter> <closedtype-parameters>

<closedtype-parameter> ::= "+" lname

<closedtype-body> ::=  | <closedtype-constructors>

<closedtype-constructors> ::=
    <closedtype-constructor> |
    <closedtype-constructor> ";" <closedtype-constructors>

<closedtype-constructor> ::=
    uname <types> anchor |
    "subtype" "closedtype" <type-const> "of" <closedtype-body> "end"

<dynamictype-constructors> ::=
    <dynamictype-constructor> |
    <dynamictype-constructor> "|" <dynamictype-constructors>

<dynamictype-constructor> ::= <type-const> | <anchor>

<types> ::=  | <type-3> <types>

<patterns> ::=  | <pattern-4> <patterns>

<pattern-1> ::= <pattern-2> | <pattern-2> ":" <type>

<pattern-2> ::= <pattern-3> | <pattern-3> "::" <pattern-2>

<pattern-3> ::= uname <patterns> | <pattern-4>

<pattern-4> ::= <pattern-5> | <pattern-5> "@" <pattern-4>

<pattern-5> ::=
    <literal> |
    <constructor> |
    <pattern-var> |
    "_" |
    "[" <comma-separated(<pattern-1>)> "]" |
    "(" ")" |
    "(" <pattern-1> "," <comma-separated-1(<pattern-1>)> ")" |
    "(" <pattern-1> ")"

<pattern-var> ::= lname

<constructor> ::= quname

<literal> ::=
    literal-number |
    literal-text
```

## Type Infix Operators

{!type-infix.md!}

## Infix Operators

{!infix.md!}

## Lexical

This is only approximate.
Block comments (inside `{#`, `#}`) nest.

```text
ignored ::=
    whitespace |
    "#" linearchar* newline |
    "{#" char* "#}"

uname ::= upper ("-" | "_" | alnum)*

quname ::= (uname ".")* uname

lname ::= lower ("-" | "_" | alnum)*

qlname ::= (uname ".")* lname

literal-number ::=
    "-"? digit+ ("." digit* ("_" digit*)?)? |
    "~" "-"? digit+ ("." digit*)? ("e" "-"? digit+)? |
    "NaN"

literal-text ::= "\"" (safechar | "\\" char)* "\""

hex64 ::= "-"* (xdigit "-"*){64}

anchor ::= "!" (literal-text|hex64)
```

## Documentation Comments

A documentation comment can be placed before any `<declaration>`.
A block documentation comment consists of a comment inside `{#|`, `#}`.
A line documentation comment consists of one or more lines starting with `#|`.
