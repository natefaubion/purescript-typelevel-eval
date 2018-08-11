module Type.Eval where

import Prim.TypeError (kind Doc, class Fail, Above, Beside, Quote, Text)
import Type.Proxy (Proxy(..))

foreign import kind TypeExpr

data TEProxy (expr :: TypeExpr) = TEProxy

proxyEval :: forall expr ty. Eval expr ty => TEProxy expr -> Proxy ty
proxyEval _ = Proxy

class Eval (expr :: TypeExpr) (result :: Type) | expr -> result

class EvalTypeError (name :: Symbol) (ctx :: Symbol) (expected :: Symbol) (ty :: Type)

infixr 2 type Beside as <>

infixr 1 type Above as |>

instance evalTypeError ::
  ( Fail
      (  Text "Error evaluating `" <> Text name <> Text "`"
      |> Text "  in " <> Text ctx <> Text ":"
      |> Text ""
      |> Text "  Expected `" <> Text expected <> Text "`"
      |> Text "   but got `" <> Quote ty <> Text "`"
      )
  ) =>
  EvalTypeError name ctx expected ty

foreign import data Throw :: Symbol -> Doc -> TypeExpr

instance throw ::
  ( Fail
      (  Text "Error evaluating `" <> Text name <> Text "`:"
      |> Text "  " <> exc
      )
  ) =>
  Eval (Throw name exc) a

foreign import data Lift :: Type -> TypeExpr

instance lift ::
  Eval (Lift a) a

foreign import data Lift1 :: (Type -> Type) -> Type -> TypeExpr

instance lift1 ::
  Eval (Lift1 f a) (f a)

foreign import data Lift2 :: (Type -> Type -> Type) -> Type -> Type -> TypeExpr

instance lift2 ::
  Eval (Lift2 f a b) (f a b)
