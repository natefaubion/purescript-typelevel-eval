module Type.Eval.Functor where

import Data.Symbol (SProxy)
import Data.Tuple (Tuple)
import Prim.RowList as RL
import Type.Proxy (Proxy)
import Type.Eval (class Eval, TypeExpr)

foreign import data Map :: (Type -> TypeExpr) -> Type -> TypeExpr

infixl 4 type Map as <$>

instance map_RowList_Cons ::
  ( Eval (fn a) b
  , Eval (Map fn (Proxy rl)) (Proxy rl')
  ) =>
  Eval (Map fn (Proxy (RL.Cons sym a rl))) (Proxy (RL.Cons sym b rl'))

instance map_RowList_Nil ::
  Eval (Map fn (Proxy RL.Nil)) (Proxy RL.Nil)

instance map_Tuple ::
  ( Eval (fn a) a'
  , Eval (fn b) b'
  ) =>
  Eval (Map fn (Tuple a b)) (Tuple a' b')

foreign import data MapWithIndex :: (Type -> Type -> TypeExpr) -> Type -> TypeExpr

instance mapWithIndex_RowList_Cons ::
  ( Eval (fn (SProxy sym) a) b
  , Eval (MapWithIndex fn (Proxy rl)) (Proxy rl')
  ) =>
  Eval (MapWithIndex fn (Proxy (RL.Cons sym a rl))) (Proxy (RL.Cons sym b rl'))

instance mapWithIndex_RowList_Nil ::
  Eval (MapWithIndex fn (Proxy RL.Nil)) (Proxy RL.Nil)
