module Type.Eval.Dispatch where

import Type.Eval (TypeExpr)

type KindOf :: forall k. k -> Type
type KindOf t = k

type KindOf1 :: forall k1 (a :: k1) (k :: k1 -> Type). k a -> (k1 -> Type)
type KindOf1 t = k

type KindOf2 :: forall k1 k2 (a :: k1) (b :: k2) (k :: k1 -> k2 -> Type). k a b -> (k1 -> k2 -> Type)
type KindOf2 t = k

type KindOf3 :: forall k1 k2 k3 (a :: k1) (b :: k2) (c :: k3) (k :: k1 -> k2 -> k3 -> Type). k a b c -> (k1 -> k2 -> k3 -> Type)
type KindOf3 t = k

type KindOf4 :: forall k1 k2 k3 k4 (a :: k1) (b :: k2) (c :: k3) (d :: k4) (k :: k1 -> k2 -> k3 -> k4 -> Type). k a b c d -> (k1 -> k2 -> k3 -> k4 -> Type)
type KindOf4 t = k

type KindOf5 :: forall k1 k2 k3 k4 k5 (a :: k1) (b :: k2) (c :: k3) (d :: k4) (e :: k5) (k :: k1 -> k2 -> k3 -> k4 -> k5 -> Type). k a b c d e -> (k1 -> k2 -> k3 -> k4 -> k5 -> Type)
type KindOf5 t = k

type KindOf6 :: forall k1 k2 k3 k4 k5 k6 (a :: k1) (b :: k2) (c :: k3) (d :: k4) (e :: k5) (f :: k6) (k :: k1 -> k2 -> k3 -> k4 -> k5 -> k6 -> Type). k a b c d e f -> (k1 -> k2 -> k3 -> k4 -> k5 -> k6 -> Type)
type KindOf6 t = k

type KindOf7 :: forall k1 k2 k3 k4 k5 k6 k7 (a :: k1) (b :: k2) (c :: k3) (d :: k4) (e :: k5) (f :: k6) (g :: k7) (k :: k1 -> k2 -> k3 -> k4 -> k5 -> k6 -> k7 -> Type). k a b c d e f g -> (k1 -> k2 -> k3 -> k4 -> k5 -> k6 -> k7 -> Type)
type KindOf7 t = k

class Dispatch1 :: forall k1 a. k1 -> TypeExpr a -> a -> Constraint
class Dispatch1 kd dispatch result | kd dispatch -> result
