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

foreign import data Bind :: (Type -> TypeExpr) -> TypeExpr -> TypeExpr

instance bind ::
  ( Eval a b
  , Eval (f b) c
  ) =>
  Eval (Bind f a) c

infixl 0 type Bind as =<<

foreign import data BindFlipped :: TypeExpr -> (Type -> TypeExpr) -> TypeExpr

instance bindFlipped ::
  ( Eval a b
  , Eval (f b) c
  ) =>
  Eval (BindFlipped a f) c

infixr 1 type BindFlipped as >>=
