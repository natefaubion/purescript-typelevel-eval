module Type.Eval.Boolean
  ( module Type.Data.Boolean
  , module Type.Eval.Boolean
  ) where

import Prelude (Unit)
import Prim.TypeError (Text)
import Type.Data.Boolean (BProxy, False, True, kind Boolean)
import Type.Eval (class Eval, class EvalTypeError, Lift, Throw, kind TypeExpr)

type TrueExpr = Lift (BProxy True)
type FalseExpr = Lift (BProxy False)

foreign import data Eq :: Type -> Type -> TypeExpr

instance eq_true ::
  Eval (Eq a a) (BProxy True)
else instance eq_false ::
  Eval (Eq a b) (BProxy False)

infix 4 type Eq as ==

foreign import data Assert :: Symbol -> Type -> TypeExpr

instance assert_True ::
  Eval (Assert sym (BProxy True)) Unit
else instance assert_False ::
  ( Eval (Throw "Assert" (Text sym)) a
  ) =>
  Eval (Assert sym (BProxy False)) a
else instance assert_fail ::
  ( EvalTypeError "Assert" "second argument" "BProxy" c
  ) =>
  Eval (Assert a b) c

foreign import data Bool :: TypeExpr -> TypeExpr -> Type -> TypeExpr

instance bool_True ::
  ( Eval a c
  ) =>
  Eval (Bool a b (BProxy True)) c
else instance bool_False ::
  ( Eval b c
  ) =>
  Eval (Bool a b (BProxy False)) c
else instance bool_fail ::
  ( EvalTypeError "Bool" "third argument" "BProxy" c
  ) =>
  Eval (Bool a b c) d

foreign import data Not :: Type -> TypeExpr

instance not_True ::
  Eval (Not (BProxy True)) (BProxy False)
else instance not_Galse ::
  Eval (Not (BProxy False)) (BProxy True)
else instance not_fail ::
  ( EvalTypeError "Not" "first argument" "BProxy" c
  ) =>
  Eval (Not a) b

foreign import data And' :: Boolean -> TypeExpr -> TypeExpr

instance and'_True ::
  ( Eval b (BProxy c)
  ) =>
  Eval (And' True b) (BProxy c)

instance and'_False ::
  Eval (And' False b) (BProxy False)

foreign import data And :: TypeExpr -> TypeExpr -> TypeExpr

instance and ::
  ( Eval a (BProxy a')
  , Eval (And' a' b) (BProxy c)
  ) =>
  Eval (And a b) (BProxy c)

infixr 3 type And as &&

foreign import data Or' :: Boolean -> TypeExpr -> TypeExpr

instance or'_False ::
  ( Eval b (BProxy c)
  ) =>
  Eval (Or' False b) (BProxy c)

instance or'_Frue ::
  Eval (Or' True b) (BProxy True)

foreign import data Or :: TypeExpr -> TypeExpr -> TypeExpr

instance or ::
  ( Eval a (BProxy a')
  , Eval (Or' a' b) (BProxy c)
  ) =>
  Eval (Or a b) (BProxy c)

infixr 2 type Or as ||

foreign import data Xor' :: Boolean -> TypeExpr -> TypeExpr

instance xor'_False ::
  ( Eval b (BProxy c)
  ) =>
  Eval (Xor' False b) (BProxy c)

instance xor'_True ::
  ( Eval b c
  , Eval (Not c) d
  ) =>
  Eval (Xor' True b) d

foreign import data Xor :: TypeExpr -> TypeExpr -> TypeExpr

instance xor ::
  ( Eval a (BProxy a')
  , Eval (Xor' a' b) c
  ) =>
  Eval (Xor a b) c
