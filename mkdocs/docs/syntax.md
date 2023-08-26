# Syntax

The syntax of the language is based on Haskell.
These are the main differences:

* Layout is not significant.
Instead, declarations within a `let` block, lines within a `do` statement, and cases within a `match` statement, are separated by `;`.
Also, `match` and `do` statements are terminated with `end`.
* There's no "top level" for declarations.
All declarations, including type declarations, are local to a `let` block.
* There's no "equation syntax" for function definitions. Use `fn`, or `match` to match argument patterns.
* There's no tuple type bigger than two. The tuple `(a,b,c)` is equivalent to `(a,(b,c))`, etc.

| Haskell | Pinafore |
| ----- | ----- |
| <code>\-\- comment</code> | <code>\# comment</code> |
| <code>\{\- comment \-\}</code> | <code>\{\# comment \#\}</code> |
| `v :: T` | `v : T` |
| <code>h : t</code> | <code>h :: t</code> |
| <code>\\x -\> x + 1</code> | <code>fn x =\> x + 1</code> |
| <code>\\case</code> | `match` |
| <code>x \& f = f x</code> | <code>x \>- f = f x</code> |
| <code>case x of</code> | <code>x \>- match</code> |
| `()` | `Unit` |
| `Bool` | `Boolean` |
| `[]` | `List` |
| `NonEmpty` | `List1` |
| <code>(P, Q)</code> | <code>P \*: Q</code> |
| <code>Either P Q</code> | <code>P +: Q</code> |
| `IO` | `Action` |


## Grammar

* A script file passed to `pinafore` has syntax `<script>`.
* Modules loaded with `import` have syntax `<module>`.
* In interactive mode, each line has syntax `<interactive>`.

```text
<script> ::= <expression>

<module> ::= <semicolon-separated(<declaration>)>

<names> ::= <comma-separated(<name>)>

<name> ::= uname | lname | "(" <infix-operator[n]> ")"

<qname> ::= quname | qlname

<interactive> ::= <do-line> | <declarator> | ":" <interactive-command>

<interactive-command> ::=
    "doc" <qname> |
    "type" <expression> |
    "simplify" "+" <type> |
    "simplify" "-" <type>

<type> :: =
    "rec" <type-var> "," <type>
    <type-0>

<type-0> :: =
    <type-1> "|" <type> |
    <type-1> "&" <type> |
    <type-1>

<type-infix> ::= "->"

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
    "fn" <match> |
    "match" <semicolon-separated(<match>)> "end" |
    <declarator> "in" <expression> |
    "if" <expression> "then" <expression> "else" <expression> |
    "do" <optional("." <namespace>)> <semicolon-separated(<do-line>)> <expression> "end" |
    <expression-2>

<expression-2> ::= <expression-3> | <expression-2> <expression-3>

<annotation> ::= "@" <type-3> | anchor

<annotations> ::= <annotation> | <annotations> <annotation>

<expression-3> ::=
    "{" <optional("." <namespace>)> <expression> "}" |
    "%" <expression-3> |
    <expression-specialform> |
    <expression-var> |
    <constructor-expression> |
    <literal> |
    "[" <comma-separated(<expression>)> "]" |
    "(" ")" |
    "(" <expression> "," <comma-separated-1(<expression>)> ")" |
    "(" <expression> ")" |
    "(" <infix-operator[n]> ")"

<constructor-expression> ::= <constructor> <optional(<of(<name> "=" <expression>)>)>

<expression-var> ::= qlname

<expression-specialform> ::= qlname <annotations>

<optional(n)> ::=  | n

<comma-separated(n)> ::=  | <comma-separated-1(n)>

<comma-separated-1(n)> ::=
    n |
    <comma-separated-1(n)> "," n

<semicolon-separated(n)> ::=  | <semicolon-separated-1(n)>

<semicolon-separated-1(n)> ::= n ";" <semicolon-separated(n)>

<of(n)> ::= "of" <semicolon-separated(n)> "end"

<match> ::= <comma-separated(<pattern>)> "=>" <expression>

<do-line> = <expression> | <pattern-1> "<-" <expression>

<declarator> ::=
    "let" <semicolon-separated(<declaration>)>
    "let" "rec" <semicolon-separated(<direct-declaration>)>
    "import" <comma-separated(<module-name>)> |
    "with" <comma-separated(<namespace> <with-names> <optional("as" <namespace>)>)> |

<declaration> ::=
    direct-declaration |
    "namespace" uname <of(<declaration>)> |
    "expose" <name-list> |
    <declarator> "end"
    <declarator> "in" <declaration> "end"

<direct-declaration> ::=
    "datatype" <type-const> <plain-datatype-parameters> <optional("<:" <supertypes>)> <of(<plain-datatype-constructor>)> |
    "datatype" "storable" <type-const> <storable-datatype-parameters> <of(<storable-datatype-constructor>)> |
    "opentype" <type-const> |
    "subtype" <optional("trustme")> <type> "<:" <type> <optional("=" <expression>)> |
    "dynamictype" <type-const> "=" <dynamictype-constructors> |
    <binding>

<name-item> ::= <name> | "namespace" <name>

<name-list> ::= <comma-separated(<name-item>)>

<with-names> ::=  | "(" <name-list> ")" | "except" "(" <name-list> ")"

<namespace> ::= uname | uname "." <namespace> | "."

<module-name> ::= literal-text

<binding> ::= <pattern-1> "=" <expression>

<supertypes> = <type> | <supertypes> "&" <type>

<plain-datatype-parameters> ::=  | <plain-datatype-parameter> <plain-datatype-parameters>

<plain-datatype-parameter> ::=
    "+" lname |
    "-" lname |
    "{" "+" lname "," "-" lname "}" |
    "{" "-" lname "," "+" lname "}" |

<plain-datatype-constructor> ::=
    uname <types> |
    uname <of(<record-member>)> |
    "subtype" "datatype" <type-const> <of(<plain-datatype-constructor>)>

<record-member> ::= lname ":" <type> <optional("=" <expression>)>

<storable-datatype-parameters> ::=  | <storable-datatype-parameter> <storable-datatype-parameters>

<storable-datatype-parameter> ::= "+" lname

<storable-datatype-constructor> ::=
    uname <types> anchor |
    "subtype" "datatype" "storable" <type-const> <of(<storable-datatype-constructor>)>

<dynamictype-constructors> ::=
    <dynamictype-constructor> |
    <dynamictype-constructor> "|" <dynamictype-constructors>

<dynamictype-constructor> ::= <type-const> | <anchor>

<types> ::=  | <type-3> <types>

<patterns> ::=  | <pattern-4> <patterns>

<pattern-1> ::= <pattern-2> | <pattern-1> ":" <type> | <pattern-1> ":?" <type> | <pattern-1> "as" <namespace>

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
    "(" <infix-operator[n]> ")" |
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

quname ::= uname ("." uname)*

lname ::= lower ("-" | "_" | alnum)*

qlname ::= lname ("." uname)*

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
