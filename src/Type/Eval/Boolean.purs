module Type.Eval.Boolean
  ( module Type.Data.Boolean
  , module Type.Eval.Boolean
  ) where

import Prelude (Unit)
import Prim.TypeError (Text)
import Type.Data.Boolean (BProxy, False, True, kind Boolean)
import Type.Eval (class Eval, class EvalTypeError, Lift, Throw, kind TypeExpr)
import Type.Eval.Function (type ($))

type TrueExpr = Lift (BProxy True)
type FalseExpr = Lift (BProxy False)

foreign import data Eq :: Type -> Type -> TypeExpr

instance eq_True ::
  Eval (Eq a a) (BProxy True)
else instance eq_False ::
  Eval (Eq a b) (BProxy False)

infix 4 type Eq as ==

foreign import data NotEq :: Type -> Type -> TypeExpr

instance notEq_False ::
  Eval (NotEq a a) (BProxy False)
else instance notEq_True ::
  Eval (NotEq a b) (BProxy True)

infix 4 type NotEq as /=

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
else instance not_False ::
  Eval (Not (BProxy False)) (BProxy True)
else instance not_fail ::
  ( EvalTypeError "Not" "first argument" "BProxy" c
  ) =>
  Eval (Not a) b

foreign import data And :: TypeExpr -> TypeExpr -> TypeExpr

instance and ::
  ( Eval (Bool b FalseExpr $ a') c
  ) =>
  Eval (And a b) c

infixr 3 type And as &&

foreign import data Or :: TypeExpr -> TypeExpr -> TypeExpr

instance or ::
  ( Eval (Bool TrueExpr b $ a) c
  ) =>
  Eval (Or a b) c

infixr 2 type Or as ||

foreign import data Xor :: TypeExpr -> TypeExpr -> TypeExpr

instance xor ::
  ( Eval (Bool (Not $ b) b $ a) c
  ) =>
  Eval (Xor a b) c
