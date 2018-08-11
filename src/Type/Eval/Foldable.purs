module Type.Eval.Foldable where

import Data.Symbol (SProxy)
import Data.Tuple (Tuple)
import Prim.RowList as RL
import Type.Data.RowList (RLProxy)
import Type.Eval (class Eval, kind TypeExpr)
import Type.Eval.Boolean (Bool, FalseExpr, TrueExpr)

foreign import data Foldr :: (Type -> TypeExpr -> TypeExpr) -> TypeExpr -> Type -> TypeExpr

instance foldr_RowList_Cons ::
  ( Eval (fn a (Foldr fn z (RLProxy rl))) ty
  ) =>
  Eval (Foldr fn z (RLProxy (RL.Cons sym a rl))) ty

instance foldr_RowList_Nil ::
  ( Eval z ty
  ) =>
  Eval (Foldr fn z (RLProxy RL.Nil)) ty

instance foldr_Tuple ::
  ( Eval (fn a (Foldr fn z b)) ty
  ) =>
  Eval (Foldr fn z (Tuple a b)) ty

foreign import data FoldrWithIndex :: (Type -> Type -> TypeExpr -> TypeExpr) -> TypeExpr -> Type -> TypeExpr

instance foldrWithIndex_RowList_Cons ::
  ( Eval (fn (SProxy sym) a (FoldrWithIndex fn z (RLProxy rl))) ty
  ) =>
  Eval (FoldrWithIndex fn z (RLProxy (RL.Cons sym a rl))) ty

instance foldrWithIndex_RowList_Nil ::
  ( Eval z ty
  ) =>
  Eval (FoldrWithIndex fn z (RLProxy RL.Nil)) ty

foreign import data AllFold :: (Type -> TypeExpr) -> Type -> TypeExpr -> TypeExpr

instance allFold ::
  ( Eval (fn a) a'
  , Eval (Bool b FalseExpr a') c
  ) =>
  Eval (AllFold fn a b) c

type All (f :: Type -> TypeExpr) =
  Foldr (AllFold f) TrueExpr

foreign import data SomeFold :: (Type -> TypeExpr) -> Type -> TypeExpr -> TypeExpr

instance someFold ::
  ( Eval (fn a) a'
  , Eval (Bool TrueExpr b a') c
  ) =>
  Eval (SomeFold fn a b) c

type Some (f :: Type -> TypeExpr) =
  Foldr (SomeFold f) FalseExpr
