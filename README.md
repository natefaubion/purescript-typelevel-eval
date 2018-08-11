# purescript-typelevel-eval

Higher-order functional programming in PureScript's type system.

Inspired by [Haskell with only one type family](https://blog.poisson.chat/posts/2018-08-06-one-type-family.html).

## What does it do?

This library exports a typeclass `Eval` which lets us evaluate type-level
expressions of kind `TypeExpr`. A `TypeExpr` evaluates to something of kind
`Type`. With this we can do lazy, higher-order functional programming
to compute types and their relationships.

## Example

Take an input row, and compute a row where all the types are `String`:

```purescript
type RowToString =
  ToRow <<< Map (Const String) <<< FromRow

test :: forall ri ro.
  Eval (RowToString ri) ro =>
  { | ri } ->
  { | ro } ->
  Unit
test _ _ = Unit
```

Assert that all rows have a type `String`:

```purescipt
type RowAllString =
  Assert "Only String is allowed" <<< All (Eq String) <<< FromRow

test :: forall ri.
  Eval (RowAllString ri) Unit =>
  { | ri } ->
  Unit
test _ = Unit
```
