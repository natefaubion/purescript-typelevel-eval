module Test.Main where

import Prelude

import Effect (Effect)
import Type.Eval (TEProxy(..), proxyEval)
import Type.Eval.Boolean (BProxy, Eq, False, True)
import Type.Eval.Foldable (All)
import Type.Eval.Function (type (<<<), Const)
import Type.Eval.Functor (Map)
import Type.Eval.RowList (FromRow, ToRow)
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

main :: Effect Unit
main = pure unit
