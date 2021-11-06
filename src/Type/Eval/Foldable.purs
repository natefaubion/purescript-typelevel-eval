module Type.Eval.Foldable where

import Data.Tuple (Tuple)
import Prim.RowList (RowList)
import Prim.RowList as RowList
import Type.Eval (class Eval, TypeExpr)
import Type.Eval.Dispatch (class Dispatch1, KindOf1)
import Type.Eval.Tuple (Tuple')
import Type.Eval.Boolean (Bool, FalseExpr, TrueExpr)

foreign import data Foldr :: forall f a b. (a -> TypeExpr b -> TypeExpr b) -> TypeExpr b -> f a -> TypeExpr b

instance
  ( Dispatch1 (KindOf1 f) (Foldr k b f) c
  ) =>
  Eval (Foldr k b f) c

instance
  ( Eval (f a (Foldr f acc tail)) acc'
  ) =>
  Dispatch1 RowList (Foldr f acc (RowList.Cons sym a tail)) acc'
else instance
  ( Eval acc acc'
  ) =>
  Dispatch1 RowList (Foldr f acc RowList.Nil) acc'

instance
  ( Eval (f b acc) acc'
  ) =>
  Dispatch1 (Tuple a') (Foldr f acc (Tuple' ((a) :: a') b)) acc'

foreign import data FoldrWithIndex :: forall f i a b. (i -> a -> TypeExpr b -> TypeExpr b) -> TypeExpr b -> f a -> TypeExpr b

instance
  ( Dispatch1 (KindOf1 f) (FoldrWithIndex k b f) c
  ) =>
  Eval (FoldrWithIndex k b f) c

instance
  ( Eval (f sym a (FoldrWithIndex f acc tail)) acc'
  ) =>
  Dispatch1 RowList (FoldrWithIndex f acc (RowList.Cons sym a tail)) acc'
else instance
  ( Eval acc acc'
  ) =>
  Dispatch1 RowList (FoldrWithIndex f acc RowList.Nil) acc'

foreign import data AllFold :: forall a. (a -> TypeExpr Boolean) -> a -> TypeExpr Boolean -> TypeExpr Boolean

instance
  ( Eval (f a) a'
  , Eval (Bool b FalseExpr a') c
  ) =>
  Eval (AllFold f a b) c

type All :: forall g a. (a -> TypeExpr Boolean) -> g a -> TypeExpr Boolean
type All f = Foldr (AllFold f) TrueExpr

foreign import data AnyFold :: forall a. (a -> TypeExpr Boolean) -> a -> TypeExpr Boolean -> TypeExpr Boolean

instance
  ( Eval (f a) a'
  , Eval (Bool TrueExpr b a') c
  ) =>
  Eval (AnyFold f a b) c

type Any :: forall g a. (a -> TypeExpr Boolean) -> g a -> TypeExpr Boolean
type Any f = Foldr (AnyFold f) FalseExpr
