module Type.Eval.Row where

import Prim.Row as Row
import Type.Proxy (Proxy)
import Type.Eval (class Eval, TypeExpr)
import Type.Eval.Function (type (<<<))

foreign import data Prj :: Symbol -> Type -> TypeExpr

instance prj ::
  ( Row.Cons sym ty rx r
  ) =>
  Eval (Prj sym (Proxy r)) ty

foreign import data Uncons :: (Type -> Type -> TypeExpr) -> Symbol -> Type -> TypeExpr

instance uncons ::
  ( Row.Cons sym ty r2 r1
  , Eval (fn ty (Proxy r2)) x
  ) =>
  Eval (Uncons fn sym (Proxy r1)) x

foreign import data Cons :: Symbol -> Type -> Type -> TypeExpr

instance cons ::
  ( Row.Cons sym ty r1 r2
  ) =>
  Eval (Cons sym ty (Proxy r1)) (Proxy r2)

type Set sym a =
  Cons sym a <<< Lacks sym

foreign import data Modify :: (Type -> TypeExpr) -> Symbol -> Type -> TypeExpr

instance modify ::
  ( Row.Cons sym ty r2 r1
  , Eval (fn ty) ty'
  , Row.Cons sym ty' r2 r3
  ) =>
  Eval (Modify fn sym (Proxy r1)) (Proxy r3)

foreign import data Nub :: Type -> TypeExpr

instance nub ::
  ( Row.Nub r1 r2
  ) =>
  Eval (Nub (Proxy r1)) (Proxy r2)

foreign import data Lacks :: Symbol -> Type -> TypeExpr

instance lacks ::
  ( Row.Lacks sym r
  ) =>
  Eval (Lacks sym (Proxy r)) (Proxy r)

foreign import data Union :: Type -> Type -> TypeExpr

instance union ::
  ( Row.Union r1 r2 r3
  ) =>
  Eval (Union (Proxy r1) (Proxy r2)) (Proxy r3)
