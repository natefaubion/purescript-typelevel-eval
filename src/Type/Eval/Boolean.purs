module Type.Eval.Boolean
  ( module Type.Eval.Boolean
  ) where

import Prelude (Unit)
import Type.Proxy (Proxy)
import Prim.TypeError (Text)
import Prim.Boolean (False, True)
import Type.Eval (class Eval, class EvalTypeError, Lift, Throw, TypeExpr)
import Type.Eval.Function (type ($))

type TrueExpr = Lift (Proxy True)
type FalseExpr = Lift (Proxy False)

foreign import data Eq :: Type -> Type -> TypeExpr

instance eq_True ::
  Eval (Eq a a) (Proxy True)
else instance eq_False ::
  Eval (Eq a b) (Proxy False)

infix 4 type Eq as ==

foreign import data NotEq :: Type -> Type -> TypeExpr

instance notEq_False ::
  Eval (NotEq a a) (Proxy False)
else instance notEq_True ::
  Eval (NotEq a b) (Proxy True)

infix 4 type NotEq as /=

foreign import data Assert :: Symbol -> Type -> TypeExpr

instance assert_True ::
  Eval (Assert sym (Proxy True)) Unit
else instance assert_False ::
  ( Eval (Throw "Assert" (Text sym)) a
  ) =>
  Eval (Assert sym (Proxy False)) a
else instance assert_fail ::
  ( EvalTypeError "Assert" "second argument" "Proxy" c
  ) =>
  Eval (Assert a b) c

foreign import data Bool :: TypeExpr -> TypeExpr -> Type -> TypeExpr

instance bool_True ::
  ( Eval a c
  ) =>
  Eval (Bool a b (Proxy True)) c
else instance bool_False ::
  ( Eval b c
  ) =>
  Eval (Bool a b (Proxy False)) c
else instance bool_fail ::
  ( EvalTypeError "Bool" "third argument" "Proxy" c
  ) =>
  Eval (Bool a b c) d

foreign import data Not :: Type -> TypeExpr

instance not_True ::
  Eval (Not (Proxy True)) (Proxy False)
else instance not_False ::
  Eval (Not (Proxy False)) (Proxy True)
else instance not_fail ::
  ( EvalTypeError "Not" "first argument" "Proxy" c
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
