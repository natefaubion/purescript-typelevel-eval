module Test.Main where

import Prelude

import Effect (Effect)
import Data.Tuple (Tuple(..))
import Type.Eval (class Eval, proxyEval, TypeExpr)
import Type.Eval.Boolean (Eq, False, Not, True)
import Type.Eval.Dispatch (class Dispatch1)
import Type.Eval.Foldable (All)
import Type.Eval.Function (type (<<<), Const)
import Type.Eval.Functor (Map)
import Type.Eval.RowList (FromRow, ToRow)
import Type.Eval.ValueOf (ValueOf)
import Type.Eval.ValueOf as ValueOf
import Type.Proxy (Proxy(..))

type Test_Map_RowList =
  ToRow <<< Map (Const String) <<< FromRow

test_Map_RowList ::
  (Proxy
    (a :: String, b :: String, c :: String))
test_Map_RowList = proxyEval
  (Proxy :: _
    (Test_Map_RowList (a :: Int, b :: Boolean, c :: Number)))

type Test_All_RowList =
  All (Eq String) <<< FromRow

test_All_RowList1 ::
  (Proxy
    True)
test_All_RowList1 = proxyEval
  (Proxy :: _
    (Test_All_RowList (a :: String, b :: String, c :: String)))

test_All_RowList2 ::
  (Proxy
    False)
test_All_RowList2 = proxyEval
  (Proxy :: _
    (Test_All_RowList (a :: String, b :: String, c :: Int)))

foreign import data Elem :: Type -> TypeExpr Type

instance Eval (Elem String) Char
instance Eval (Elem (Array a)) a

testValueOfString :: ValueOf (Elem String)
testValueOfString = ValueOf.from 'a'

testFromValueOfString :: Char
testFromValueOfString = ValueOf.to testValueOfString

testValueOfArray :: ValueOf (Elem (Array Int))
testValueOfArray = ValueOf.from 1

testFromValueOfArray :: Int
testFromValueOfArray = ValueOf.to testValueOfArray

foreign import data F :: forall k. Boolean -> TypeExpr (k -> Type)

instance Eval (F True) Record
instance Eval (F False) (Tuple String)

testValueOfFalse :: ValueOf (F False) Int
testValueOfFalse = ValueOf.from (Tuple "foo" 42)

testValueOfTrue :: ValueOf (F True) (foo :: Int)
testValueOfTrue = ValueOf.from { foo: 42 }

testFromValueOfFalse :: Tuple String String
testFromValueOfFalse = ValueOf.to (show <$> testValueOfFalse)

foreign import data I :: forall k. k -> TypeExpr k

instance Eval (I a) a

data D a = D (ValueOf (I Tuple) String a)

derive instance Functor D

foreign import data C :: Type -> Type
foreign import data C1 :: forall k. k -> C k

instance Eval (f a) b => Dispatch1 C (Map f (C1 a)) (C1 b)

testMapC :: Proxy (C1 False)
testMapC = proxyEval (Proxy :: _ (Map Not (C1 True)))

main :: Effect Unit
main = pure unit
