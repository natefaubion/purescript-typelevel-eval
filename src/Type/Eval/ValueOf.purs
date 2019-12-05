module Type.Eval.ValueOf where

import Data.Leibniz (Leibniz(..))
import Type.Eval (class Eval, kind TypeExpr)
import Unsafe.Coerce (unsafeCoerce)

data ValueOf (expr :: TypeExpr)

toValueOf :: forall expr a. Eval expr a => a -> ValueOf expr
toValueOf = unsafeToValueOf

unsafeToValueOf :: forall expr a. a -> ValueOf expr
unsafeToValueOf = unsafeCoerce

valueOf :: forall expr a. Eval expr a => ValueOf expr -> a
valueOf = unsafeCoerce

toLeibniz :: forall expr a. Eval expr a => Leibniz (ValueOf expr) a
toLeibniz = Leibniz unsafeCoerce

