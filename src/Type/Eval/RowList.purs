module Type.Eval.RowList where

import Prim.RowList (class RowToList)
import Type.Data.RowList (RLProxy)
import Type.Eval (class Eval, kind TypeExpr)
import Type.Row (class ListToRow, RProxy)

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
