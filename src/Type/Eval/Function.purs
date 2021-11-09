module Type.Eval.Function where

import Type.Eval (class Eval, TypeExpr)

foreign import data Const :: forall k. k -> k -> TypeExpr k

instance Eval (Const a b) a

foreign import data Compose :: forall a b c. (b -> TypeExpr c) -> (a -> TypeExpr b) -> a -> TypeExpr c

instance
  ( Eval (g a) b
  , Eval (f b) c
  ) =>
  Eval (Compose f g a) c

infixr 9 type Compose as <<<

foreign import data ComposeFlipped :: forall a b c. (a -> TypeExpr b) -> (b -> TypeExpr c) -> a -> TypeExpr c

instance
  ( Eval (f a) b
  , Eval (g b) c
  ) =>
  Eval (ComposeFlipped f g a) c

infixr 9 type ComposeFlipped as >>>

foreign import data Flip :: forall a b c. (a -> b -> TypeExpr c) -> b -> a -> TypeExpr c

instance
  ( Eval (f b a) c
  ) =>
  Eval (Flip f a b) c

foreign import data App :: forall a b. (a -> TypeExpr b) -> TypeExpr a -> TypeExpr b

instance
  ( Eval a b
  , Eval (f b) c
  ) =>
  Eval (App f a) c

infixr 0 type App as $

foreign import data AppFlipped :: forall a b. TypeExpr a -> (a -> TypeExpr b) -> TypeExpr b

instance
  ( Eval a b
  , Eval (f b) c
  ) =>
  Eval (AppFlipped a f) c

infixl 1 type AppFlipped as #
