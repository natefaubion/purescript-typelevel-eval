module Type.Eval.Tuple
  ( module Data.Tuple
  , module Type.Eval.Tuple
  ) where

import Data.Tuple (Tuple)
import Type.Eval (class Eval, kind TypeExpr)

foreign import data Fst :: Type -> TypeExpr

instance fst ::
  Eval (Fst (Tuple a b)) a

foreign import data Snd :: Type -> TypeExpr

instance snd ::
  Eval (Snd (Tuple a b)) b

foreign import data Duplicate :: Type -> TypeExpr

instance duplicate ::
  Eval (Duplicate a) (Tuple a a)

foreign import data Curry :: (Type -> Type) -> Type -> Type -> TypeExpr

instance curry ::
  Eval (Curry f a b) (f (Tuple a b))

foreign import data Uncurry :: (Type -> Type -> Type) -> Type -> TypeExpr

instance uncurry ::
  Eval (Uncurry f (Tuple a b)) (f a b)
