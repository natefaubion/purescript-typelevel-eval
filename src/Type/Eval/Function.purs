module Type.Eval.Function where

import Type.Eval (class Eval, kind TypeExpr)

foreign import data Id :: Type -> TypeExpr

instance id ::
  Eval (Id a) a

foreign import data Const :: Type -> Type -> TypeExpr

instance const ::
  Eval (Const a b) a

foreign import data Compose :: (Type -> TypeExpr) -> (Type -> TypeExpr) -> Type -> TypeExpr

instance compose ::
  ( Eval (g a) b
  , Eval (f b) c
  ) =>
  Eval (Compose f g a) c

infixr 9 type Compose as <<<

foreign import data ComposeFlipped :: (Type -> TypeExpr) -> (Type -> TypeExpr) -> Type -> TypeExpr

instance composeFlipped ::
  ( Eval (f a) b
  , Eval (g b) c
  ) =>
  Eval (ComposeFlipped f g a) c

infixr 9 type ComposeFlipped as >>>

foreign import data Flip :: (Type -> Type -> TypeExpr) -> Type -> Type -> TypeExpr

instance flip ::
  ( Eval (f b a) c
  ) =>
  Eval (Flip f a b) c

foreign import data App :: (Type -> TypeExpr) -> TypeExpr -> TypeExpr

instance appSeq ::
  ( Eval a b
  , Eval (f b) c
  ) =>
  Eval (App f a) c

infixr 0 type App as $

foreign import data AppFlipped :: TypeExpr -> (Type -> TypeExpr) -> TypeExpr

instance appFlipped ::
  ( Eval a b
  , Eval (f b) c
  ) =>
  Eval (AppFlipped a f) c

infixl 1 type AppFlipped as #
