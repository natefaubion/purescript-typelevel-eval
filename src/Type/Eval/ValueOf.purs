module Type.Eval.ValueOf where

import Data.Leibniz (Leibniz(..))
import Type.Eval (class Eval, TypeExpr)
import Unsafe.Coerce (unsafeCoerce)

data ValueOf (expr :: TypeExpr)

from :: forall expr a. Eval expr a => a -> ValueOf expr
from = unsafeCoerce

unsafeFrom :: forall expr a. a -> ValueOf expr
unsafeFrom = unsafeCoerce

to :: forall expr a. Eval expr a => ValueOf expr -> a
to = unsafeCoerce

asLeibniz :: forall expr a. Eval expr a => Leibniz (ValueOf expr) a
asLeibniz = Leibniz unsafeCoerce

