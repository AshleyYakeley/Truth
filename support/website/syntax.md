# Syntax

The syntax of the language is based on Haskell.
These are the most obvious differences:

* Layout is not significant.
Instead, declarations within a `let` block, lines within a `do` statement, and cases within a `fn { ... }` expression, are separated by semicolons.
These blocks are enclosed in braces.
* A script is an expression; declarations can be introduced with `let { ... }`.
Module files contain top-level declarations.
* There's no "equation syntax" for function definitions. Use `fn` to match argument patterns.
* There's no tuple type bigger than two. The tuple `(a,b,c)` is equivalent to `(a,(b,c))`, etc.

| Haskell | Pinafore |
| ----- | ----- |
| <code>\-\- comment</code> | <code>\# comment</code> |
| <code>\{\- comment \-\}</code> | <code>\{\# comment \#\}</code> |
| `v :: T` | `v : T` |
| <code>h : t</code> | <code>h :: t</code> |
| <code>\\x -\> x + 1</code> | <code>fn x =\> x + 1</code> |
| <code>\\case</code> | `fn { ... }` |
| <code>x \& f = f x</code> | <code>x \>- f = f x</code> |
| <code>case x of</code> | <code>x \>- fn { ... }</code> |
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

This is a sketch of the token grammar; whitespace and comments are omitted.
Comma-separated lists do not allow a trailing comma; semicolon-separated lists allow a trailing semicolon.
A `do` block must be nonempty and end with an expression.
All cases in a braced `fn` must have the same number of argument patterns.

In the infix productions, `n` ranges from 1 to 6 for types and 1 to 11 for expressions.
Smaller precedence numbers bind more tightly.
Operators at the same precedence must have compatible associativity; nonassociative operators cannot be chained.
A signed or range-valued `<type-operand>` must be an operand of a type infix operator:
both `<type>` and `<type-1>` must yield a single unsigned type.
The operands of `|` and `&` must likewise be single unsigned types.
Thus `(+A, -B) -> C` is allowed, but `(+A, -B)` alone is not.
Recursive types are allowed directly as infix operands and as operands of `|` and `&`,
for example `a -> rec b, Maybe b` and `a | rec b, Maybe b`.
The body after `rec a,` is a full `<type>` and extends as far to the right as possible.
A recursive type used as a prefix type argument still needs parentheses, as in `WholeModel (rec a, Maybe a)`.
The unqualified type names `Any` and `None` denote the top and bottom types.

```text
<script> ::= <expression>

<module> ::= <semicolon-separated(<declaration>)>

<qname> ::= quname | qlname | operator

<interactive> ::=  | <do-line> | ":" <interactive-command>

<interactive-command> ::=
    "doc" <qname> |
    "t" <expression> |
    "type" <expression> |
    "info" <expression> |
    "simplify" "+" <type> |
    "simplify" "-" <type> |
    "simplify-" <type>

<type> ::= <type-infix[6]>

<type-infix[n]> ::=
    <type-infix[n-1]> |
    <type-infix[n]> <type-infix-operator[n,left]> <type-infix[n-1]> |
    <type-infix[n-1]> <type-infix-operator[n,right]> <type-infix[n]>

<type-infix[0]> ::= <type-operand>

<type-operand> ::=
    <type-operand-atom> "|" <type-1> |
    <type-operand-atom> "&" <type-1> |
    <type-operand-atom>

<type-operand-atom> ::=
    <type-2> |
    "(" <comma-separated(<type-range-item>)> ")" |
    "-" <type-3> |
    "+" <type-3>

<type-1> ::= <type-operand>

<type-infix-operator[n,dir]> ::= -- see table

<type-2> ::=
    "rec" <type-var> "," <type> |
    <type-const> <list-1(<type-argument>)> |
    <type-3>

<type-argument> ::=
    <type-3> |
    "(" <comma-separated(<type-range-item>)> ")" |
    "-" <type-3> |
    "+" <type-3>

<type-3> ::=
    "(" <type> ")" |
    <type-var> |
    <type-const>

<type-range-item> ::=
    <type> |
    "-" <type> |
    "+" <type>

<type-var> ::= lname

<type-const> ::= quname

<expression> ::=
    <expression-infix[11]> |
    <expression> ":" <type>

<expression-infix[n]> ::=
    <expression-infix[n-1]> |
    <expression-infix[n]> <infix-operator[n,left]> <expression-infix[n-1]> |
    <expression-infix[n-1]> <infix-operator[n,right]> <expression-infix[n]> |
    <expression-infix[n-1]> <infix-operator[n,none]> <expression-infix[n-1]>

<expression-infix[0]> ::= <expression-1>

<infix-operator[n,dir]> ::= -- see table

<expression-1> ::=
    "fn" <match> |
    "fn" <braced(<match>)> |
    "imply" <braced(<implication>)> <expression> |
    <declarator> <expression> |
    "if" <expression> "then" <expression> "else" <expression> |
    "do" <optional("." <namespace>)> <braced(<do-line>)> |
    <expression-2>

<expression-2> ::= <expression-3> | <expression-2> <expression-3>

<expression-3> ::=
    "%" <expression-3> |
    "ap" <optional("." <namespace>)> "{" <expression> "}" |
    <splice> |
    "!expression" "{" <expression> "}" |
    "!scope" <braced(<declaration>)> |
    "@" <type-3> |
    anchor |
    <expression-var> |
    implicit-name |
    <constructor-expression> |
    <literal> |
    "[" <comma-separated(<expression>)> "]" |
    "(" ")" |
    "(" <expression> "," <comma-separated-1(<expression>)> ")" |
    "(" <expression> ")" |
    "(" operator ")"

<implication> ::= implicit-name <optional(":" <type>)> "=" <expression>

<constructor-expression> ::= <constructor> <optional(<braced(lname "=" <expression>)>)>

<expression-var> ::= qlname <optional(<braced(lname "=" <expression>)>)>

<splice> ::= "!{" <expression> "}"

<optional(n)> ::=  | n

<list(n)> ::=  | <list-1(n)>

<list-1(n)> ::= n <list(n)>

<comma-separated(n)> ::=  | <comma-separated-1(n)>

<comma-separated-1(n)> ::= n | n "," <comma-separated-1(n)>

<semicolon-separated(n)> ::=  | <semicolon-separated-1(n)>

<semicolon-separated-1(n)> ::= n | n ";" <semicolon-separated(n)>

<braced(n)> ::= "{" <semicolon-separated(n)> "}"

<match> ::= <comma-separated(<pattern-1>)> "=>" <expression>

<do-line> ::=
    <expression> |
    <declaration> |
    <pattern-1> "<-" <expression>

<declarator> ::=
    "let" <braced(<declaration>)> |
    "let" "rec" <braced(<direct-declaration>)> |
    "import" <comma-separated(<module-name>)> |
    "with" <comma-separated(<namespace> <with-names> <optional("as" <namespace>)>)>

<declaration> ::=
    <direct-declaration> |
    "type" <type-const> <plain-datatype-parameters> "=" <type> |
    "type" "storable" <type-const> <plain-datatype-parameters> "=" <type> |
    "predicatetype" <optional("storable")> <type-const> "<:" <type> "=" <expression> |
    <record-binding> |
    <splice> |
    "namespace" <optional("docsec")> uname <braced(<declaration>)> |
    "docsec" literal-text <braced(<declaration>)> |
    "expose" <name-list> |
    <declarator> |
    <declarator> <declaration>

<direct-declaration> ::=
    "datatype" <type-const> <plain-datatype-parameters> <optional("<:" <type>)> <braced(<plain-datatype-constructor>)> |
    "datatype" "storable" <type-const> <plain-datatype-parameters> <braced(<storable-datatype-constructor>)> |
    "entitytype" <type-const> |
    "subtype" <optional("trustme")> <type> "<:" <type> <optional("=" <expression>)> |
    <binding>

<name-item> ::= <qname> | "namespace" <namespace>

<name-list> ::= <comma-separated(<name-item>)>

<with-names> ::=  | "{" <name-list> "}" | "except" "{" <name-list> "}"

<namespace> ::= quname

<module-name> ::= literal-text

<binding> ::= <pattern-1> "=" <expression>

<record-binding> ::= lname <braced(<record-member>)> <optional(":" <type>)> "=" <expression>

<plain-datatype-parameters> ::=  | <plain-datatype-parameter> <plain-datatype-parameters>

<plain-datatype-parameter> ::=
    "+" lname |
    "-" lname |
    "(" "+" lname "," "-" lname ")" |
    "(" "-" lname "," "+" lname ")" |
    lname

<plain-datatype-constructor> ::=
    quname <types> |
    quname <braced(<record-member>)> |
    "subtype" "datatype" <type-const> <braced(<plain-datatype-constructor>)>

<record-member> ::= lname ":" <type> <optional("=" <expression>)> | quname

<storable-datatype-constructor> ::=
    quname <types> anchor |
    "subtype" "datatype" "storable" <type-const> <braced(<storable-datatype-constructor>)>

<types> ::=  | <type-3> <types>

<pattern-1> ::= <pattern-2> | <pattern-1> ":" <type> | <pattern-1> ":?" <type> | <pattern-1> "as" <namespace>

<pattern-2> ::= <pattern-3> | <pattern-3> "::" <pattern-2>

<pattern-3> ::= <pattern-constructor> <list-1(<pattern-4>)> | <pattern-4>

<pattern-4> ::= <pattern-5> | <pattern-5> "@" <pattern-4>

<pattern-5> ::=
    <pattern-constructor> |
    <pattern-var> |
    "_" |
    "[" <comma-separated(<pattern-1>)> "]" |
    "(" ")" |
    "(" operator ")" |
    "(" <pattern-1> "," <comma-separated-1(<pattern-1>)> ")" |
    "(" <pattern-1> ")"

<pattern-var> ::= qlname

<pattern-constructor> ::= <constructor> | <literal>

<constructor> ::= quname

<literal> ::=
    literal-number |
    literal-text
```

## Type Infix Operators

Unlisted type operators associate to the left at precedence 2.
The operators `+` and `-` introduce signed arguments instead of acting as type infix operators.
The dedicated `|` and `&` tokens bind more tightly than type infix operators and associate to the right at the same level.


```{include} generated/type-infix.md
```

## Infix Operators

Unlisted expression operators associate to the left at precedence 1.
The operators `~==` and `~/=` are also nonassociative at precedence 6.


```{include} generated/infix.md
```

## Lexical

This is only approximate.
Block comments (inside `{#`, `#}`) nest.
Reserved keywords and the standalone `_` are not name tokens.
A trailing dot makes a qualified name absolute.
The `operator` token includes qualified operators (such as `+.N` or `+.N.`), but excludes reserved punctuation such as `=`, `=>`, `:`, `|`, and `&`.
Its characters are drawn from `!$%&*+./<=>?@\^|-~:` and non-ASCII symbols or punctuation.

```text
ignored ::=
    whitespace |
    "#" linearchar* newline |
    "{#" char* "#}"

uname ::= upper ("-" | "_" | alnum)*

quname ::= uname ("." uname)* "."?

lname ::= (lower | "_") ("-" | "_" | alnum)*

qlname ::= lname ("." uname)* "."?

implicit-name ::= "?" lname

literal-number ::=
    "-"? digit+ ("." digit* ("_" digit*)?)? |
    "-"? digit+ "/" digit+ |
    "~Infinity" |
    "~-Infinity" |
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
Datatype constructors and record members also accept documentation comments.
