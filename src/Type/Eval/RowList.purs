module Type.Eval.RowList where

import Prim.RowList (class RowToList)
import Type.Eval (class Eval, TypeExpr)
import Type.Proxy (Proxy)
import Type.RowList (class ListToRow)

foreign import data FromRow :: Type -> TypeExpr

instance fromRow ::
  ( RowToList r rl
  ) =>
  Eval (FromRow (Proxy r)) (Proxy rl)

foreign import data ToRow :: Type -> TypeExpr

instance toRow ::
  ( ListToRow rl r
  ) =>
  Eval (ToRow (Proxy rl)) (Proxy r)
