module Type.Eval.Tuple
  ( module Data.Tuple
  , module Type.Eval.Tuple
  ) where

import Data.Tuple (Tuple)
import Type.Eval (class Eval, TypeExpr)

foreign import data Tuple' :: forall a b. a -> b -> Tuple a b

foreign import data Fst :: forall a b. Tuple a b -> TypeExpr a

instance Eval (Fst (Tuple' a b)) a

foreign import data Snd :: forall a b. Tuple a b -> TypeExpr b

instance Eval (Snd (Tuple' a b)) b

foreign import data Duplicate :: forall a. a -> TypeExpr (Tuple a a)

instance Eval (Duplicate a) (Tuple' a a)

foreign import data Curry :: forall a b c. (Tuple a b -> TypeExpr c) -> a -> b -> TypeExpr c

instance Eval (f (Tuple' a b)) c => Eval (Curry f a b) c

foreign import data Uncurry :: forall a b c. (a -> b -> TypeExpr c) -> Tuple a b -> TypeExpr c

instance Eval (f a b) c => Eval (Uncurry f (Tuple' a b)) c
