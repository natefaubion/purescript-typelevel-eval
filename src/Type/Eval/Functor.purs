module Type.Eval.Functor where

import Data.Tuple (Tuple)
import Prim.RowList (RowList)
import Prim.RowList as RowList
import Type.Eval (class Eval, TypeExpr)
import Type.Eval.Dispatch (class Dispatch1, KindOf1)
import Type.Eval.Tuple (Tuple')

foreign import data Map :: forall f a b. (a -> TypeExpr b) -> f a -> TypeExpr (f b)

instance
  ( Dispatch1 (KindOf1 f) (Map k f) g
  ) =>
  Eval (Map k f) g

infixl 4 type Map as <$>

instance
  ( Eval (f a) a'
  , Eval (Map f tail) tail'
  ) =>
  Dispatch1 RowList (Map f (RowList.Cons sym a tail)) (RowList.Cons sym a' tail')
else instance
  Dispatch1 RowList (Map f RowList.Nil) RowList.Nil

instance
  ( Eval (f b) b'
  ) =>
  Dispatch1 (Tuple a') (Map f (Tuple' ((a) :: a') b)) (Tuple' ((a) :: a') b')

foreign import data MapWithIndex :: forall f i a b. (i -> a -> TypeExpr b) -> f a -> TypeExpr (f b)

instance
  ( Eval (fn sym a) a'
  , Eval (MapWithIndex f tail') tail'
  ) =>
  Dispatch1 RowList (MapWithIndex f (RowList.Cons sym a tail)) (RowList.Cons sym a' tail')
else instance
  Dispatch1 RowList (MapWithIndex f RowList.Nil) RowList.Nil
