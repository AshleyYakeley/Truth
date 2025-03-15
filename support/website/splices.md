# Quotes and Splices

A _macro_ is, informally, an expression best used in splices.
Such an expression typically has a type of the form `A -> B -> ... Interpreter Expression`.
(Note most of these names are actually in the `Pinafore.` namespace.)

You can create macros with quotes, as well as with ordinary Pinafore code.
You can use macros in splices.

## Quotes

Quotes are ways of representing Pinafore code for later use in splices, or to execute at run-time.
You can think of them as "code literals", just as `"this"` is a text literal.

* Given a type `T`, `@T` has type `OpenType T` (note subtype relation `OpenType () <: Type`).
* Given an expression `e`, `!expression {e}` has type `Interpreter Expression`.
* Given declarations `d`, `!declarations {d}` has type `Interpreter Declarations`.

## Splices

Splices are how you can use macros.

* Given `expr: Interpreter Expression`, `!{expr}` in expression context is an expression.
* Given `expr: Interpreter Declarations`, `!{expr}` in declaration context is bindings.
* Given `expr: Interpreter Type`, `!{expr}` in type context is a type.

For example:

* `check` has type `Type -> Interpreter Expression`. `!{check @Integer}` of type `Literal -> Maybe Integer` determines whether a given `Literal` is an `Integer`.
* `point.OpenEntity` has type `Type -> Anchor -> Interpreter Expression`. `point.OpenEntity @MyEntity !"somepoint"` is a point entity for the anchor `!"somepoint"` of type `MyEntity`.

## Run-Time Evaluation

You can run `Interpreter` values with `run.Interpreter`.
You can obtain a `Context` for this either with `!{this.Context}`, or creating your own.
