module Test.Main where

import Prelude

import Effect (Effect)
import Type.Eval (class Eval, TEProxy(..), proxyEval, TypeExpr)
import Type.Eval.Boolean (Eq)
import Prim.Boolean (False, True)
import Type.Eval.Foldable (All)
import Type.Eval.Function (type (<<<), Const)
import Type.Eval.Functor (Map)
import Type.Eval.RowList (FromRow, ToRow)
import Type.Eval.ValueOf (ValueOf)
import Type.Eval.ValueOf as ValueOf
import Type.Proxy (Proxy)

type Test_Map_RowList =
  ToRow <<< Map (Const String) <<< FromRow

test_Map_RowList ::
  (Proxy
    (Proxy (a :: String, b :: String, c :: String)))
test_Map_RowList = proxyEval
  (TEProxy :: TEProxy
    (Test_Map_RowList (Proxy (a :: Int, b :: Boolean, c :: Number))))

type Test_All_RowList =
  All (Eq String) <<< FromRow

test_All_RowList1 ::
  (Proxy
    (Proxy True))
test_All_RowList1 = proxyEval
  (TEProxy :: TEProxy
    (Test_All_RowList (Proxy (a :: String, b :: String, c :: String))))

test_All_RowList2 ::
  (Proxy
    (Proxy False))
test_All_RowList2 = proxyEval
  (TEProxy :: TEProxy
    (Test_All_RowList (Proxy (a :: String, b :: String, c :: Int))))

foreign import data Elem :: Type -> TypeExpr

instance evalElemString :: Eval (Elem String) Char

instance evalElemArray :: Eval (Elem (Array a)) a

testValueOfString :: ValueOf (Elem String)
testValueOfString = ValueOf.from 'a'

testFromValueOfString :: Char
testFromValueOfString = ValueOf.to testValueOfString

testValueOfArray :: ValueOf (Elem (Array Int))
testValueOfArray = ValueOf.from 1

testFromValueOfArray :: Int
testFromValueOfArray = ValueOf.to testValueOfArray

main :: Effect Unit
main = pure unit
