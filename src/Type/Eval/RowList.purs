module Type.Eval.RowList where

import Prim.RowList (class RowToList)
import Type.Eval (class Eval, kind TypeExpr)
import Type.Prelude (RLProxy, RProxy)
import Type.RowList (class ListToRow)

foreign import data FromRow :: Type -> TypeExpr

instance fromRow ::
  ( RowToList r rl
  ) =>
  Eval (FromRow (RProxy r)) (RLProxy rl)

foreign import data ToRow :: Type -> TypeExpr

instance toRow ::
  ( ListToRow rl r
  ) =>
  Eval (ToRow (RLProxy rl)) (RProxy r)
