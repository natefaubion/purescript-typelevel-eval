module Test.Main where

import Prelude

import Effect (Effect)
import Type.Eval (class Eval, TEProxy(..), proxyEval, kind TypeExpr)
import Type.Eval.Boolean (BProxy, Eq, False, True)
import Type.Eval.Foldable (All)
import Type.Eval.Function (type (<<<), Const)
import Type.Eval.Functor (Map)
import Type.Eval.RowList (FromRow, ToRow)
import Type.Eval.ValueOf (ValueOf, fromValueOf, toValueOf)
import Type.Proxy (Proxy)
import Type.Row (RProxy)

type Test_Map_RowList =
  ToRow <<< Map (Const String) <<< FromRow

test_Map_RowList ::
  (Proxy
    (RProxy (a :: String, b :: String, c :: String)))
test_Map_RowList = proxyEval
  (TEProxy :: TEProxy
    (Test_Map_RowList (RProxy (a :: Int, b :: Boolean, c :: Number))))

type Test_All_RowList =
  All (Eq String) <<< FromRow

test_All_RowList1 ::
  (Proxy
    (BProxy True))
test_All_RowList1 = proxyEval
  (TEProxy :: TEProxy
    (Test_All_RowList (RProxy (a :: String, b :: String, c :: String))))

test_All_RowList2 ::
  (Proxy
    (BProxy False))
test_All_RowList2 = proxyEval
  (TEProxy :: TEProxy
    (Test_All_RowList (RProxy (a :: String, b :: String, c :: Int))))

foreign import data Elem :: Type -> TypeExpr

instance evalElemString :: Eval (Elem String) Char

instance evalElemArray :: Eval (Elem (Array a)) a

testValueOfString :: ValueOf (Elem String)
testValueOfString = toValueOf 'a'

testFromValueOfString :: Char
testFromValueOfString = fromValueOf testValueOfString

testValueOfArray :: ValueOf (Elem (Array Int))
testValueOfArray = toValueOf 1

testFromValueOfArray :: Int
testFromValueOfArray = fromValueOf testValueOfArray

main :: Effect Unit
main = pure unit
