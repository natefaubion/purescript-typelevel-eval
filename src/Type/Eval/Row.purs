module Type.Eval.Row where

import Prim.Row as Row
import Type.Data.Row (RProxy)
import Type.Eval (class Eval, kind TypeExpr)
import Type.Eval.Function (type (<<<))

foreign import data Prj :: Symbol -> Type -> TypeExpr

instance prj ::
  ( Row.Cons sym ty rx r
  ) =>
  Eval (Prj sym (RProxy r)) ty

foreign import data Uncons :: (Type -> Type -> TypeExpr) -> Symbol -> Type -> TypeExpr

instance uncons ::
  ( Row.Cons sym ty r2 r1
  , Eval (fn ty (RProxy r2)) x
  ) =>
  Eval (Uncons fn sym (RProxy r1)) x

foreign import data Cons :: Symbol -> Type -> Type -> TypeExpr

instance cons ::
  ( Row.Cons sym ty r1 r2
  ) =>
  Eval (Cons sym ty (RProxy r1)) (RProxy r2)

type Set sym a =
  Cons sym a <<< Lacks sym

foreign import data Modify :: (Type -> TypeExpr) -> Symbol -> Type -> TypeExpr

instance modify ::
  ( Row.Cons sym ty r2 r1
  , Eval (fn ty) ty'
  , Row.Cons sym ty' r2 r3
  ) =>
  Eval (Modify fn sym (RProxy r1)) (RProxy r3)

foreign import data Nub :: Type -> TypeExpr

instance nub ::
  ( Row.Nub r1 r2
  ) =>
  Eval (Nub (RProxy r1)) (RProxy r2)

foreign import data Lacks :: Symbol -> Type -> TypeExpr

instance lacks ::
  ( Row.Lacks sym r
  ) =>
  Eval (Lacks sym (RProxy r)) (RProxy r)

foreign import data Union :: Type -> Type -> TypeExpr

instance union ::
  ( Row.Union r1 r2 r3
  ) =>
  Eval (Union (RProxy r1) (RProxy r2)) (RProxy r3)
