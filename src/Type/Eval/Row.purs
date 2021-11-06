module Type.Eval.Row where

import Prim.Row as Row
import Type.Eval (class Eval, TypeExpr)
import Type.Eval.Function (Const, type (<<<))

foreign import data Prj :: forall k. Symbol -> Row k -> TypeExpr k

instance
  ( Row.Cons sym ty rx r
  ) =>
  Eval (Prj sym r) ty

foreign import data Uncons :: forall k r. (k -> Row k -> TypeExpr r) -> Symbol -> Row k -> TypeExpr r

instance
  ( Row.Cons sym ty r2 r1
  , Eval (fn ty r2) x
  ) =>
  Eval (Uncons fn sym r1) x

foreign import data Cons :: forall k. Symbol -> k -> Row k -> TypeExpr (Row k)

instance
  ( Row.Cons sym ty r1 r2
  ) =>
  Eval (Cons sym ty r1) r2

type Insert :: forall k. Symbol -> k -> Row k -> TypeExpr (Row k)
type Insert sym a = Cons sym a <<< Lacks sym

type Set :: forall k. Symbol -> k -> Row k -> TypeExpr (Row k)
type Set sym a = Modify (Const a) sym

foreign import data Modify :: forall k. (k -> TypeExpr k) -> Symbol -> Row k -> TypeExpr (Row k)

instance
  ( Row.Cons sym ty r2 r1
  , Eval (fn ty) ty'
  , Row.Cons sym ty' r2 r3
  ) =>
  Eval (Modify fn sym r1) r3

foreign import data Nub :: forall k. Row k -> TypeExpr (Row k)

instance
  ( Row.Nub r1 r2
  ) =>
  Eval (Nub r1) r2

foreign import data Lacks :: forall k. Symbol -> Row k -> TypeExpr (Row k)

instance
  ( Row.Lacks sym r
  ) =>
  Eval (Lacks sym r) r

foreign import data Union :: forall k. Row k -> Row k -> TypeExpr (Row k)

instance
  ( Row.Union r1 r2 r3
  ) =>
  Eval (Union r1 r2) r3
