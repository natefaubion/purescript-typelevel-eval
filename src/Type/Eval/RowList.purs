module Type.Eval.RowList where

import Prim.RowList (class RowToList, RowList)
import Type.Eval (class Eval, TypeExpr)
import Type.RowList (class ListToRow)

foreign import data FromRow :: forall k. Row k -> TypeExpr (RowList k)

instance
  ( RowToList r rl
  ) =>
  Eval (FromRow r) rl

foreign import data ToRow :: forall k. RowList k -> TypeExpr (Row k)

instance
  ( ListToRow rl r
  ) =>
  Eval (ToRow rl) r
