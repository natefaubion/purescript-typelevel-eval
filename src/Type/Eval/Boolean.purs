module Type.Eval.Boolean
  ( module Type.Data.Boolean
  , module Type.Eval.Boolean
  ) where

import Prelude (Unit)
import Prim.TypeError (Text)
import Type.Data.Boolean (False, True)
import Type.Eval (class Eval, TypeExpr, Lift, Throw)
import Type.Eval.Unit (Unit')

type TrueExpr = Lift True

type FalseExpr = Lift False

foreign import data Eq :: forall a. a -> a -> TypeExpr Boolean

instance
  Eval (Eq a a) True
else instance
  Eval (Eq a b) False

infix 4 type Eq as ==

foreign import data NotEq :: forall a. a -> a -> TypeExpr Boolean

instance
  Eval (NotEq a a) False
else instance
  Eval (NotEq a b) True

infix 4 type NotEq as /=

foreign import data Assert :: Symbol -> Boolean -> TypeExpr Unit

instance
  Eval (Assert sym True) Unit'
else instance
  ( Eval (Throw "Assert" (Text sym)) a
  ) =>
  Eval (Assert sym False) a

foreign import data Bool :: forall a. TypeExpr a -> TypeExpr a -> Boolean -> TypeExpr a

instance
  ( Eval a c
  ) =>
  Eval (Bool a b True) c
else instance
  ( Eval b c
  ) =>
  Eval (Bool a b False) c

foreign import data Not :: Boolean -> TypeExpr Boolean

instance
  Eval (Not True) False
else instance
  Eval (Not False) True

foreign import data And :: TypeExpr Boolean -> TypeExpr Boolean -> TypeExpr Boolean

instance
  ( Eval a a'
  , Eval (Bool b FalseExpr a') c
  ) =>
  Eval (And a b) c

infixr 3 type And as &&

foreign import data Or :: TypeExpr Boolean -> TypeExpr Boolean -> TypeExpr Boolean

instance
  ( Eval a a'
  , Eval (Bool TrueExpr b a') c
  ) =>
  Eval (Or a b) c

infixr 2 type Or as ||

foreign import data Xor :: TypeExpr Boolean -> TypeExpr Boolean -> TypeExpr Boolean

instance xor ::
  ( Eval a a'
  , Eval b b'
  , Eval (Bool (Not b') b a') c
  ) =>
  Eval (Xor a b) c
