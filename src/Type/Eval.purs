module Type.Eval where

import Prim.TypeError (Doc, class Fail, Above, Beside, Quote, Text)
import Type.Proxy (Proxy(..))

foreign import data TypeExpr :: forall k. k -> Type

proxyEval :: forall expr ty. Eval expr ty => Proxy expr -> Proxy ty
proxyEval _ = Proxy

class Eval :: forall k. TypeExpr k -> k -> Constraint
class Eval expr result | expr -> result

infixr 2 type Beside as <>

infixr 1 type Above as |>

foreign import data TypeError :: forall a b c. Symbol -> Symbol -> a -> b -> TypeExpr c

instance
  ( Fail
      ( Text "Error evaluating `" <> Text name <> Text "`"
          |> Text "  in " <> Text ctx <> Text ":"
          |> Text ""
          |> Text "  Expected `" <> Text expected <> Text "`"
          |> Text "   but got `" <> Quote ty <> Text "`"
      )
  ) =>
  Eval (TypeError name ctx expected ty) z

foreign import data Throw :: forall a. Symbol -> Doc -> TypeExpr a

instance
  ( Fail
      ( Text "Error evaluating `" <> Text name <> Text "`:"
          |> Text "  " <> exc
      )
  ) =>
  Eval (Throw name exc) a

foreign import data Lift :: forall k. k -> TypeExpr k

instance Eval (Lift a) a
