module Type.Eval.ValueOf
  ( ValueOf
  , class IsValueOf
  , from
  , to
  , over
  , modify
  , asLeibniz
  ) where

import Prelude

import Control.Alt (class Alt, alt)
import Control.Alternative (class Alternative)
import Control.Biapplicative (class Biapplicative, bipure)
import Control.Biapply (class Biapply, biapply)
import Control.Comonad (class Comonad, extract)
import Control.Extend (class Extend, extend)
import Control.Plus (class Plus, empty)
import Data.Bifunctor (class Bifunctor, bimap)
import Data.Either (Either)
import Data.Functor.Contravariant (class Contravariant, cmap)
import Data.HeytingAlgebra (ff, implies, tt)
import Data.Leibniz (Leibniz(..))
import Data.Profunctor (class Profunctor, dimap)
import Data.Profunctor.Choice (class Choice, left, right)
import Data.Profunctor.Closed (class Closed, closed)
import Data.Profunctor.Strong (class Strong, first, second)
import Data.Tuple (Tuple)
import Prim.TypeError (class Fail, Above, Beside, Quote, Text)
import Type.Eval (class Eval, TypeExpr)
import Unsafe.Coerce (unsafeCoerce)

foreign import data ValueOf :: forall k. TypeExpr k -> k

class IsValueOf :: Type -> Type -> Constraint
class IsValueOf a b | a -> b

instance Eval x f => IsValueOf (ValueOf x a b c d e g h) (f a b c d e g h)
else instance Eval x f => IsValueOf (ValueOf x a b c d e g) (f a b c d e g)
else instance Eval x f => IsValueOf (ValueOf x a b c d e) (f a b c d e)
else instance Eval x f => IsValueOf (ValueOf x a b c d) (f a b c d)
else instance Eval x f => IsValueOf (ValueOf x a b c) (f a b c)
else instance Eval x f => IsValueOf (ValueOf x a b) (f a b)
else instance Eval x f => IsValueOf (ValueOf x a) (f a)
else instance Eval x a => IsValueOf (ValueOf x) a
else instance Fail (Above (Text "Expected ValueOf (with up to 7 type variables), but got:") (Beside (Text "  ") (Quote a))) => IsValueOf a x

from :: forall a b. IsValueOf a b => b -> a
from = unsafeCoerce

to :: forall a b. IsValueOf a b => a -> b
to = unsafeCoerce

over :: forall a b c d. IsValueOf a b => IsValueOf d c => (b -> c) -> a -> d
over = unsafeCoerce

modify :: forall a b. IsValueOf a b => (b -> b) -> a -> a
modify = unsafeCoerce

asLeibniz :: forall a b. IsValueOf a b => Leibniz a b
asLeibniz = Leibniz unsafeCoerce

instance (Eval expr a, Eq a) => Eq (ValueOf expr) where
  eq = unsafeCoerce (eq :: a -> a -> Boolean)

instance (Eval expr f, Eq (f a)) => Eq (ValueOf expr a) where
  eq = unsafeCoerce (eq :: f a -> f a -> Boolean)

instance (Eval expr f, Eq (f a b)) => Eq (ValueOf expr a b) where
  eq = unsafeCoerce (eq :: f a b -> f a b -> Boolean)

instance (Eval expr f, Eq (f a b c)) => Eq (ValueOf expr a b c) where
  eq = unsafeCoerce (eq :: f a b c -> f a b c -> Boolean)

instance (Eval expr f, Eq (f a b c d)) => Eq (ValueOf expr a b c d) where
  eq = unsafeCoerce (eq :: f a b c d -> f a b c d -> Boolean)

instance (Eval expr f, Eq (f a b c d e)) => Eq (ValueOf expr a b c d e) where
  eq = unsafeCoerce (eq :: f a b c d e -> f a b c d e -> Boolean)

instance (Eval expr f, Eq (f a b c d e g)) => Eq (ValueOf expr a b c d e g) where
  eq = unsafeCoerce (eq :: f a b c d e g -> f a b c d e g -> Boolean)

instance (Eval expr f, Eq (f a b c d e g h)) => Eq (ValueOf expr a b c d e g h) where
  eq = unsafeCoerce (eq :: f a b c d e g h -> f a b c d e g h -> Boolean)

instance (Eval expr a, Ord a) => Ord (ValueOf expr) where
  compare = unsafeCoerce (compare :: a -> a -> Ordering)

instance (Eval expr f, Ord (f a)) => Ord (ValueOf expr a) where
  compare = unsafeCoerce (compare :: f a -> f a -> Ordering)

instance (Eval expr f, Ord (f a b)) => Ord (ValueOf expr a b) where
  compare = unsafeCoerce (compare :: f a b -> f a b -> Ordering)

instance (Eval expr f, Ord (f a b c)) => Ord (ValueOf expr a b c) where
  compare = unsafeCoerce (compare :: f a b c -> f a b c -> Ordering)

instance (Eval expr f, Ord (f a b c d)) => Ord (ValueOf expr a b c d) where
  compare = unsafeCoerce (compare :: f a b c d -> f a b c d -> Ordering)

instance (Eval expr f, Ord (f a b c d e)) => Ord (ValueOf expr a b c d e) where
  compare = unsafeCoerce (compare :: f a b c d e -> f a b c d e -> Ordering)

instance (Eval expr f, Ord (f a b c d e g)) => Ord (ValueOf expr a b c d e g) where
  compare = unsafeCoerce (compare :: f a b c d e g -> f a b c d e g -> Ordering)

instance (Eval expr f, Ord (f a b c d e g h)) => Ord (ValueOf expr a b c d e g h) where
  compare = unsafeCoerce (compare :: f a b c d e g h -> f a b c d e g h -> Ordering)

instance (Eval expr a, Bounded a) => Bounded (ValueOf expr) where
  top = unsafeCoerce (top :: a)
  bottom = unsafeCoerce (bottom :: a)

instance (Eval expr f, Bounded (f a)) => Bounded (ValueOf expr a) where
  top = unsafeCoerce (top :: f a)
  bottom = unsafeCoerce (bottom :: f a)

instance (Eval expr f, Bounded (f a b)) => Bounded (ValueOf expr a b) where
  top = unsafeCoerce (top :: f a b)
  bottom = unsafeCoerce (bottom :: f a b)

instance (Eval expr f, Bounded (f a b c)) => Bounded (ValueOf expr a b c) where
  top = unsafeCoerce (top :: f a b c)
  bottom = unsafeCoerce (bottom :: f a b c)

instance (Eval expr f, Bounded (f a b c d)) => Bounded (ValueOf expr a b c d) where
  top = unsafeCoerce (top :: f a b c d)
  bottom = unsafeCoerce (bottom :: f a b c d)

instance (Eval expr f, Bounded (f a b c d e)) => Bounded (ValueOf expr a b c d e) where
  top = unsafeCoerce (top :: f a b c d e)
  bottom = unsafeCoerce (bottom :: f a b c d e)

instance (Eval expr f, Bounded (f a b c d e g)) => Bounded (ValueOf expr a b c d e g) where
  top = unsafeCoerce (top :: f a b c d e g)
  bottom = unsafeCoerce (bottom :: f a b c d e g)

instance (Eval expr f, Bounded (f a b c d e g h)) => Bounded (ValueOf expr a b c d e g h) where
  top = unsafeCoerce (top :: f a b c d e g h)
  bottom = unsafeCoerce (bottom :: f a b c d e g h)

instance (Eval expr a, Show a) => Show (ValueOf expr) where
  show = unsafeCoerce (show :: a -> String)

instance (Eval expr f, Show (f a)) => Show (ValueOf expr a) where
  show = unsafeCoerce (show :: f a -> String)

instance (Eval expr f, Show (f a b)) => Show (ValueOf expr a b) where
  show = unsafeCoerce (show :: f a b -> String)

instance (Eval expr f, Show (f a b c)) => Show (ValueOf expr a b c) where
  show = unsafeCoerce (show :: f a b c -> String)

instance (Eval expr f, Show (f a b c d)) => Show (ValueOf expr a b c d) where
  show = unsafeCoerce (show :: f a b c d -> String)

instance (Eval expr f, Show (f a b c d e)) => Show (ValueOf expr a b c d e) where
  show = unsafeCoerce (show :: f a b c d e -> String)

instance (Eval expr f, Show (f a b c d e g)) => Show (ValueOf expr a b c d e g) where
  show = unsafeCoerce (show :: f a b c d e g -> String)

instance (Eval expr f, Show (f a b c d e g h)) => Show (ValueOf expr a b c d e g h) where
  show = unsafeCoerce (show :: f a b c d e g h -> String)

instance (Eval expr a, Semigroup a) => Semigroup (ValueOf expr) where
  append = unsafeCoerce (append :: a -> a -> a)

instance (Eval expr f, Semigroup (f a)) => Semigroup (ValueOf expr a) where
  append = unsafeCoerce (append :: f a -> f a -> f a)

instance (Eval expr f, Semigroup (f a b)) => Semigroup (ValueOf expr a b) where
  append = unsafeCoerce (append :: f a b -> f a b -> f a b)

instance (Eval expr f, Semigroup (f a b c)) => Semigroup (ValueOf expr a b c) where
  append = unsafeCoerce (append :: f a b c -> f a b c -> f a b c)

instance (Eval expr f, Semigroup (f a b c d)) => Semigroup (ValueOf expr a b c d) where
  append = unsafeCoerce (append :: f a b c d -> f a b c d -> f a b c d)

instance (Eval expr f, Semigroup (f a b c d e)) => Semigroup (ValueOf expr a b c d e) where
  append = unsafeCoerce (append :: f a b c d e -> f a b c d e -> f a b c d e)

instance (Eval expr f, Semigroup (f a b c d e g)) => Semigroup (ValueOf expr a b c d e g) where
  append = unsafeCoerce (append :: f a b c d e g -> f a b c d e g -> f a b c d e g)

instance (Eval expr f, Semigroup (f a b c d e g h)) => Semigroup (ValueOf expr a b c d e g h) where
  append = unsafeCoerce (append :: f a b c d e g h -> f a b c d e g h -> f a b c d e g h)

instance (Eval expr a, Monoid a) => Monoid (ValueOf expr) where
  mempty = unsafeCoerce (mempty :: a)

instance (Eval expr f, Monoid (f a)) => Monoid (ValueOf expr a) where
  mempty = unsafeCoerce (mempty :: f a)

instance (Eval expr f, Monoid (f a b)) => Monoid (ValueOf expr a b) where
  mempty = unsafeCoerce (mempty :: f a b)

instance (Eval expr f, Monoid (f a b c)) => Monoid (ValueOf expr a b c) where
  mempty = unsafeCoerce (mempty :: f a b c)

instance (Eval expr f, Monoid (f a b c d)) => Monoid (ValueOf expr a b c d) where
  mempty = unsafeCoerce (mempty :: f a b c d)

instance (Eval expr f, Monoid (f a b c d e)) => Monoid (ValueOf expr a b c d e) where
  mempty = unsafeCoerce (mempty :: f a b c d e)

instance (Eval expr f, Monoid (f a b c d e g)) => Monoid (ValueOf expr a b c d e g) where
  mempty = unsafeCoerce (mempty :: f a b c d e g)

instance (Eval expr f, Monoid (f a b c d e g h)) => Monoid (ValueOf expr a b c d e g h) where
  mempty = unsafeCoerce (mempty :: f a b c d e g h)

instance (Eval expr a, HeytingAlgebra a) => HeytingAlgebra (ValueOf expr) where
  ff = unsafeCoerce (ff :: a)
  tt = unsafeCoerce (tt :: a)
  implies = unsafeCoerce (implies :: a -> a -> a)
  conj = unsafeCoerce (conj :: a -> a -> a)
  disj = unsafeCoerce (disj :: a -> a -> a)
  not = unsafeCoerce (not :: a -> a)

instance (Eval expr f, HeytingAlgebra (f a)) => HeytingAlgebra (ValueOf expr a) where
  ff = unsafeCoerce (ff :: f a)
  tt = unsafeCoerce (tt :: f a)
  implies = unsafeCoerce (implies :: f a -> f a -> f a)
  conj = unsafeCoerce (conj :: f a -> f a -> f a)
  disj = unsafeCoerce (disj :: f a -> f a -> f a)
  not = unsafeCoerce (not :: f a -> f a)

instance (Eval expr f, HeytingAlgebra (f a b)) => HeytingAlgebra (ValueOf expr a b) where
  ff = unsafeCoerce (ff :: f a b)
  tt = unsafeCoerce (tt :: f a b)
  implies = unsafeCoerce (implies :: f a b -> f a b -> f a b)
  conj = unsafeCoerce (conj :: f a b -> f a b -> f a b)
  disj = unsafeCoerce (disj :: f a b -> f a b -> f a b)
  not = unsafeCoerce (not :: f a b -> f a b)

instance (Eval expr f, HeytingAlgebra (f a b c)) => HeytingAlgebra (ValueOf expr a b c) where
  ff = unsafeCoerce (ff :: f a b c)
  tt = unsafeCoerce (tt :: f a b c)
  implies = unsafeCoerce (implies :: f a b c -> f a b c -> f a b c)
  conj = unsafeCoerce (conj :: f a b c -> f a b c -> f a b c)
  disj = unsafeCoerce (disj :: f a b c -> f a b c -> f a b c)
  not = unsafeCoerce (not :: f a b c -> f a b c)

instance (Eval expr f, HeytingAlgebra (f a b c d)) => HeytingAlgebra (ValueOf expr a b c d) where
  ff = unsafeCoerce (ff :: f a b c d)
  tt = unsafeCoerce (tt :: f a b c d)
  implies = unsafeCoerce (implies :: f a b c d -> f a b c d -> f a b c d)
  conj = unsafeCoerce (conj :: f a b c d -> f a b c d -> f a b c d)
  disj = unsafeCoerce (disj :: f a b c d -> f a b c d -> f a b c d)
  not = unsafeCoerce (not :: f a b c d -> f a b c d)

instance (Eval expr f, HeytingAlgebra (f a b c d e)) => HeytingAlgebra (ValueOf expr a b c d e) where
  ff = unsafeCoerce (ff :: f a b c d e)
  tt = unsafeCoerce (tt :: f a b c d e)
  implies = unsafeCoerce (implies :: f a b c d e -> f a b c d e -> f a b c d e)
  conj = unsafeCoerce (conj :: f a b c d e -> f a b c d e -> f a b c d e)
  disj = unsafeCoerce (disj :: f a b c d e -> f a b c d e -> f a b c d e)
  not = unsafeCoerce (not :: f a b c d e -> f a b c d e)

instance (Eval expr f, HeytingAlgebra (f a b c d e g)) => HeytingAlgebra (ValueOf expr a b c d e g) where
  ff = unsafeCoerce (ff :: f a b c d e g)
  tt = unsafeCoerce (tt :: f a b c d e g)
  implies = unsafeCoerce (implies :: f a b c d e g -> f a b c d e g -> f a b c d e g)
  conj = unsafeCoerce (conj :: f a b c d e g -> f a b c d e g -> f a b c d e g)
  disj = unsafeCoerce (disj :: f a b c d e g -> f a b c d e g -> f a b c d e g)
  not = unsafeCoerce (not :: f a b c d e g -> f a b c d e g)

instance (Eval expr f, HeytingAlgebra (f a b c d e g h)) => HeytingAlgebra (ValueOf expr a b c d e g h) where
  ff = unsafeCoerce (ff :: f a b c d e g h)
  tt = unsafeCoerce (tt :: f a b c d e g h)
  implies = unsafeCoerce (implies :: f a b c d e g h -> f a b c d e g h -> f a b c d e g h)
  conj = unsafeCoerce (conj :: f a b c d e g h -> f a b c d e g h -> f a b c d e g h)
  disj = unsafeCoerce (disj :: f a b c d e g h -> f a b c d e g h -> f a b c d e g h)
  not = unsafeCoerce (not :: f a b c d e g h -> f a b c d e g h)

instance (Eval expr a, BooleanAlgebra a) => BooleanAlgebra (ValueOf expr)

instance (Eval expr f, BooleanAlgebra (f a)) => BooleanAlgebra (ValueOf expr a)

instance (Eval expr f, BooleanAlgebra (f a b)) => BooleanAlgebra (ValueOf expr a b)

instance (Eval expr f, BooleanAlgebra (f a b c)) => BooleanAlgebra (ValueOf expr a b c)

instance (Eval expr f, BooleanAlgebra (f a b c d)) => BooleanAlgebra (ValueOf expr a b c d)

instance (Eval expr f, BooleanAlgebra (f a b c d e)) => BooleanAlgebra (ValueOf expr a b c d e)

instance (Eval expr f, BooleanAlgebra (f a b c d e g)) => BooleanAlgebra (ValueOf expr a b c d e g)

instance (Eval expr f, BooleanAlgebra (f a b c d e g h)) => BooleanAlgebra (ValueOf expr a b c d e g h)

instance (Eval expr a, Semiring a) => Semiring (ValueOf expr) where
  add = unsafeCoerce (add :: a -> a -> a)
  zero = unsafeCoerce (zero :: a)
  mul = unsafeCoerce (mul :: a -> a -> a)
  one = unsafeCoerce (one :: a)

instance (Eval expr f, Semiring (f a)) => Semiring (ValueOf expr a) where
  add = unsafeCoerce (add :: f a -> f a -> f a)
  zero = unsafeCoerce (zero :: f a)
  mul = unsafeCoerce (mul :: f a -> f a -> f a)
  one = unsafeCoerce (one :: f a)

instance (Eval expr f, Semiring (f a b)) => Semiring (ValueOf expr a b) where
  add = unsafeCoerce (add :: f a b -> f a b -> f a b)
  zero = unsafeCoerce (zero :: f a b)
  mul = unsafeCoerce (mul :: f a b -> f a b -> f a b)
  one = unsafeCoerce (one :: f a b)

instance (Eval expr f, Semiring (f a b c)) => Semiring (ValueOf expr a b c) where
  add = unsafeCoerce (add :: f a b c -> f a b c -> f a b c)
  zero = unsafeCoerce (zero :: f a b c)
  mul = unsafeCoerce (mul :: f a b c -> f a b c -> f a b c)
  one = unsafeCoerce (one :: f a b c)

instance (Eval expr f, Semiring (f a b c d)) => Semiring (ValueOf expr a b c d) where
  add = unsafeCoerce (add :: f a b c d -> f a b c d -> f a b c d)
  zero = unsafeCoerce (zero :: f a b c d)
  mul = unsafeCoerce (mul :: f a b c d -> f a b c d -> f a b c d)
  one = unsafeCoerce (one :: f a b c d)

instance (Eval expr f, Semiring (f a b c d e)) => Semiring (ValueOf expr a b c d e) where
  add = unsafeCoerce (add :: f a b c d e -> f a b c d e -> f a b c d e)
  zero = unsafeCoerce (zero :: f a b c d e)
  mul = unsafeCoerce (mul :: f a b c d e -> f a b c d e -> f a b c d e)
  one = unsafeCoerce (one :: f a b c d e)

instance (Eval expr f, Semiring (f a b c d e g)) => Semiring (ValueOf expr a b c d e g) where
  add = unsafeCoerce (add :: f a b c d e g -> f a b c d e g -> f a b c d e g)
  zero = unsafeCoerce (zero :: f a b c d e g)
  mul = unsafeCoerce (mul :: f a b c d e g -> f a b c d e g -> f a b c d e g)
  one = unsafeCoerce (one :: f a b c d e g)

instance (Eval expr f, Semiring (f a b c d e g h)) => Semiring (ValueOf expr a b c d e g h) where
  add = unsafeCoerce (add :: f a b c d e g h -> f a b c d e g h -> f a b c d e g h)
  zero = unsafeCoerce (zero :: f a b c d e g h)
  mul = unsafeCoerce (mul :: f a b c d e g h -> f a b c d e g h -> f a b c d e g h)
  one = unsafeCoerce (one :: f a b c d e g h)

instance (Eval expr a, Ring a) => Ring (ValueOf expr) where
  sub = unsafeCoerce (sub :: a -> a -> a)

instance (Eval expr f, Ring (f a)) => Ring (ValueOf expr a) where
  sub = unsafeCoerce (sub :: f a -> f a -> f a)

instance (Eval expr f, Ring (f a b)) => Ring (ValueOf expr a b) where
  sub = unsafeCoerce (sub :: f a b -> f a b -> f a b)

instance (Eval expr f, Ring (f a b c)) => Ring (ValueOf expr a b c) where
  sub = unsafeCoerce (sub :: f a b c -> f a b c -> f a b c)

instance (Eval expr f, Ring (f a b c d)) => Ring (ValueOf expr a b c d) where
  sub = unsafeCoerce (sub :: f a b c d -> f a b c d -> f a b c d)

instance (Eval expr f, Ring (f a b c d e)) => Ring (ValueOf expr a b c d e) where
  sub = unsafeCoerce (sub :: f a b c d e -> f a b c d e -> f a b c d e)

instance (Eval expr f, Ring (f a b c d e g)) => Ring (ValueOf expr a b c d e g) where
  sub = unsafeCoerce (sub :: f a b c d e g -> f a b c d e g -> f a b c d e g)

instance (Eval expr f, Ring (f a b c d e g h)) => Ring (ValueOf expr a b c d e g h) where
  sub = unsafeCoerce (sub :: f a b c d e g h -> f a b c d e g h -> f a b c d e g h)

instance (Eval expr a, CommutativeRing a) => CommutativeRing (ValueOf expr)

instance (Eval expr f, CommutativeRing (f a)) => CommutativeRing (ValueOf expr a)

instance (Eval expr f, CommutativeRing (f a b)) => CommutativeRing (ValueOf expr a b)

instance (Eval expr f, CommutativeRing (f a b c)) => CommutativeRing (ValueOf expr a b c)

instance (Eval expr f, CommutativeRing (f a b c d)) => CommutativeRing (ValueOf expr a b c d)

instance (Eval expr f, CommutativeRing (f a b c d e)) => CommutativeRing (ValueOf expr a b c d e)

instance (Eval expr f, CommutativeRing (f a b c d e g)) => CommutativeRing (ValueOf expr a b c d e g)

instance (Eval expr f, CommutativeRing (f a b c d e g h)) => CommutativeRing (ValueOf expr a b c d e g h)

instance (Eval expr a, DivisionRing a) => DivisionRing (ValueOf expr) where
  recip = unsafeCoerce (recip :: a -> a)

instance (Eval expr f, DivisionRing (f a)) => DivisionRing (ValueOf expr a) where
  recip = unsafeCoerce (recip :: f a -> f a)

instance (Eval expr f, DivisionRing (f a b)) => DivisionRing (ValueOf expr a b) where
  recip = unsafeCoerce (recip :: f a b -> f a b)

instance (Eval expr f, DivisionRing (f a b c)) => DivisionRing (ValueOf expr a b c) where
  recip = unsafeCoerce (recip :: f a b c -> f a b c)

instance (Eval expr f, DivisionRing (f a b c d)) => DivisionRing (ValueOf expr a b c d) where
  recip = unsafeCoerce (recip :: f a b c d -> f a b c d)

instance (Eval expr f, DivisionRing (f a b c d e)) => DivisionRing (ValueOf expr a b c d e) where
  recip = unsafeCoerce (recip :: f a b c d e -> f a b c d e)

instance (Eval expr f, DivisionRing (f a b c d e g)) => DivisionRing (ValueOf expr a b c d e g) where
  recip = unsafeCoerce (recip :: f a b c d e g -> f a b c d e g)

instance (Eval expr f, DivisionRing (f a b c d e g h)) => DivisionRing (ValueOf expr a b c d e g h) where
  recip = unsafeCoerce (recip :: f a b c d e g h -> f a b c d e g h)

instance (Eval expr f, Functor f) => Functor (ValueOf expr) where
  map = unsafeCoerce (map :: forall a b. (a -> b) -> f a -> f b)

instance (Eval expr f, Functor (f a)) => Functor (ValueOf expr a) where
  map = unsafeCoerce (map :: forall x y. (x -> y) -> f a x -> f a y)

instance (Eval expr f, Functor (f a b)) => Functor (ValueOf expr a b) where
  map = unsafeCoerce (map :: forall x y. (x -> y) -> f a b x -> f a b y)

instance (Eval expr f, Functor (f a b c)) => Functor (ValueOf expr a b c) where
  map = unsafeCoerce (map :: forall x y. (x -> y) -> f a b c x -> f a b c y)

instance (Eval expr f, Functor (f a b c d)) => Functor (ValueOf expr a b c d) where
  map = unsafeCoerce (map :: forall x y. (x -> y) -> f a b c d x -> f a b c d y)

instance (Eval expr f, Functor (f a b c d e)) => Functor (ValueOf expr a b c d e) where
  map = unsafeCoerce (map :: forall x y. (x -> y) -> f a b c d e x -> f a b c d e y)

instance (Eval expr f, Functor (f a b c d e g)) => Functor (ValueOf expr a b c d e g) where
  map = unsafeCoerce (map :: forall x y. (x -> y) -> f a b c d e g x -> f a b c d e g y)

instance (Eval expr f, Apply f) => Apply (ValueOf expr) where
  apply = unsafeCoerce (apply :: forall a b. f (a -> b) -> f a -> f b)

instance (Eval expr f, Apply (f a)) => Apply (ValueOf expr a) where
  apply = unsafeCoerce (apply :: forall x y. f a (x -> y) -> f a x -> f a y)

instance (Eval expr f, Apply (f a b)) => Apply (ValueOf expr a b) where
  apply = unsafeCoerce (apply :: forall x y. f a b (x -> y) -> f a b x -> f a b y)

instance (Eval expr f, Apply (f a b c)) => Apply (ValueOf expr a b c) where
  apply = unsafeCoerce (apply :: forall x y. f a b c (x -> y) -> f a b c x -> f a b c y)

instance (Eval expr f, Apply (f a b c d)) => Apply (ValueOf expr a b c d) where
  apply = unsafeCoerce (apply :: forall x y. f a b c d (x -> y) -> f a b c d x -> f a b c d y)

instance (Eval expr f, Apply (f a b c d e)) => Apply (ValueOf expr a b c d e) where
  apply = unsafeCoerce (apply :: forall x y. f a b c d e (x -> y) -> f a b c d e x -> f a b c d e y)

instance (Eval expr f, Apply (f a b c d e g)) => Apply (ValueOf expr a b c d e g) where
  apply = unsafeCoerce (apply :: forall x y. f a b c d e g (x -> y) -> f a b c d e g x -> f a b c d e g y)

instance (Eval expr f, Applicative f) => Applicative (ValueOf expr) where
  pure = unsafeCoerce (pure :: forall a. a -> f a)

instance (Eval expr f, Applicative (f a)) => Applicative (ValueOf expr a) where
  pure = unsafeCoerce (pure :: forall x. x -> f a x)

instance (Eval expr f, Applicative (f a b)) => Applicative (ValueOf expr a b) where
  pure = unsafeCoerce (pure :: forall x. x -> f a b x)

instance (Eval expr f, Applicative (f a b c)) => Applicative (ValueOf expr a b c) where
  pure = unsafeCoerce (pure :: forall x. x -> f a b c x)

instance (Eval expr f, Applicative (f a b c d)) => Applicative (ValueOf expr a b c d) where
  pure = unsafeCoerce (pure :: forall x. x -> f a b c d x)

instance (Eval expr f, Applicative (f a b c d e)) => Applicative (ValueOf expr a b c d e) where
  pure = unsafeCoerce (pure :: forall x. x -> f a b c d e x)

instance (Eval expr f, Applicative (f a b c d e g)) => Applicative (ValueOf expr a b c d e g) where
  pure = unsafeCoerce (pure :: forall x. x -> f a b c d e g x)

instance (Eval expr f, Bind f) => Bind (ValueOf expr) where
  bind = unsafeCoerce (bind :: forall a b. f a -> (a -> f b) -> f b)

instance (Eval expr f, Bind (f a)) => Bind (ValueOf expr a) where
  bind = unsafeCoerce (bind :: forall x y. f a x -> (x -> f a y) -> f a y)

instance (Eval expr f, Bind (f a b)) => Bind (ValueOf expr a b) where
  bind = unsafeCoerce (bind :: forall x y. f a b x -> (x -> f a b y) -> f a b y)

instance (Eval expr f, Bind (f a b c)) => Bind (ValueOf expr a b c) where
  bind = unsafeCoerce (bind :: forall x y. f a b c x -> (x -> f a b c y) -> f a b c y)

instance (Eval expr f, Bind (f a b c d)) => Bind (ValueOf expr a b c d) where
  bind = unsafeCoerce (bind :: forall x y. f a b c d x -> (x -> f a b c d y) -> f a b c d y)

instance (Eval expr f, Bind (f a b c d e)) => Bind (ValueOf expr a b c d e) where
  bind = unsafeCoerce (bind :: forall x y. f a b c d e x -> (x -> f a b c d e y) -> f a b c d e y)

instance (Eval expr f, Bind (f a b c d e g)) => Bind (ValueOf expr a b c d e g) where
  bind = unsafeCoerce (bind :: forall x y. f a b c d e g x -> (x -> f a b c d e g y) -> f a b c d e g y)

instance (Eval expr f, Monad f) => Monad (ValueOf expr)

instance (Eval expr f, Monad (f a)) => Monad (ValueOf expr a)

instance (Eval expr f, Monad (f a b)) => Monad (ValueOf expr a b)

instance (Eval expr f, Monad (f a b c)) => Monad (ValueOf expr a b c)

instance (Eval expr f, Monad (f a b c d)) => Monad (ValueOf expr a b c d)

instance (Eval expr f, Monad (f a b c d e)) => Monad (ValueOf expr a b c d e)

instance (Eval expr f, Monad (f a b c d e g)) => Monad (ValueOf expr a b c d e g)

instance (Eval expr f, Alt f) => Alt (ValueOf expr) where
  alt = unsafeCoerce (alt :: forall a. f a -> f a -> f a)

instance (Eval expr f, Alt (f a)) => Alt (ValueOf expr a) where
  alt = unsafeCoerce (alt :: forall x. f a x -> f a x -> f a x)

instance (Eval expr f, Alt (f a b)) => Alt (ValueOf expr a b) where
  alt = unsafeCoerce (alt :: forall x. f a b x -> f a b x -> f a b x)

instance (Eval expr f, Alt (f a b c)) => Alt (ValueOf expr a b c) where
  alt = unsafeCoerce (alt :: forall x. f a b c x -> f a b c x -> f a b c x)

instance (Eval expr f, Alt (f a b c d)) => Alt (ValueOf expr a b c d) where
  alt = unsafeCoerce (alt :: forall x. f a b c d x -> f a b c d x -> f a b c d x)

instance (Eval expr f, Alt (f a b c d e)) => Alt (ValueOf expr a b c d e) where
  alt = unsafeCoerce (alt :: forall x. f a b c d e x -> f a b c d e x -> f a b c d e x)

instance (Eval expr f, Alt (f a b c d e g)) => Alt (ValueOf expr a b c d e g) where
  alt = unsafeCoerce (alt :: forall x. f a b c d e g x -> f a b c d e g x -> f a b c d e g x)

instance (Eval expr f, Plus f) => Plus (ValueOf expr) where
  empty = unsafeCoerce (empty :: forall a. f a)

instance (Eval expr f, Plus (f a)) => Plus (ValueOf expr a) where
  empty = unsafeCoerce (empty :: forall x. f a x)

instance (Eval expr f, Plus (f a b)) => Plus (ValueOf expr a b) where
  empty = unsafeCoerce (empty :: forall x. f a b x)

instance (Eval expr f, Plus (f a b c)) => Plus (ValueOf expr a b c) where
  empty = unsafeCoerce (empty :: forall x. f a b c x)

instance (Eval expr f, Plus (f a b c d)) => Plus (ValueOf expr a b c d) where
  empty = unsafeCoerce (empty :: forall x. f a b c d x)

instance (Eval expr f, Plus (f a b c d e)) => Plus (ValueOf expr a b c d e) where
  empty = unsafeCoerce (empty :: forall x. f a b c d e x)

instance (Eval expr f, Plus (f a b c d e g)) => Plus (ValueOf expr a b c d e g) where
  empty = unsafeCoerce (empty :: forall x. f a b c d e g x)

instance (Eval expr f, Alternative f) => Alternative (ValueOf expr)

instance (Eval expr f, Alternative (f a)) => Alternative (ValueOf expr a)

instance (Eval expr f, Alternative (f a b)) => Alternative (ValueOf expr a b)

instance (Eval expr f, Alternative (f a b c)) => Alternative (ValueOf expr a b c)

instance (Eval expr f, Alternative (f a b c d)) => Alternative (ValueOf expr a b c d)

instance (Eval expr f, Alternative (f a b c d e)) => Alternative (ValueOf expr a b c d e)

instance (Eval expr f, Alternative (f a b c d e g)) => Alternative (ValueOf expr a b c d e g)

instance (Eval expr f, Extend f) => Extend (ValueOf expr) where
  extend = unsafeCoerce (extend :: forall a b. (f a -> b) -> f a -> f b)

instance (Eval expr f, Extend (f a)) => Extend (ValueOf expr a) where
  extend = unsafeCoerce (extend :: forall x y. (f a x -> y) -> f a x -> f a y)

instance (Eval expr f, Extend (f a b)) => Extend (ValueOf expr a b) where
  extend = unsafeCoerce (extend :: forall x y. (f a b x -> y) -> f a b x -> f a b y)

instance (Eval expr f, Extend (f a b c)) => Extend (ValueOf expr a b c) where
  extend = unsafeCoerce (extend :: forall x y. (f a b c x -> y) -> f a b c x -> f a b c y)

instance (Eval expr f, Extend (f a b c d)) => Extend (ValueOf expr a b c d) where
  extend = unsafeCoerce (extend :: forall x y. (f a b c d x -> y) -> f a b c d x -> f a b c d y)

instance (Eval expr f, Extend (f a b c d e)) => Extend (ValueOf expr a b c d e) where
  extend = unsafeCoerce (extend :: forall x y. (f a b c d e x -> y) -> f a b c d e x -> f a b c d e y)

instance (Eval expr f, Extend (f a b c d e g)) => Extend (ValueOf expr a b c d e g) where
  extend = unsafeCoerce (extend :: forall x y. (f a b c d e g x -> y) -> f a b c d e g x -> f a b c d e g y)

instance (Eval expr f, Comonad f) => Comonad (ValueOf expr) where
  extract = unsafeCoerce (extract :: forall a. f a -> a)

instance (Eval expr f, Comonad (f a)) => Comonad (ValueOf expr a) where
  extract = unsafeCoerce (extract :: forall x. f a x -> x)

instance (Eval expr f, Comonad (f a b)) => Comonad (ValueOf expr a b) where
  extract = unsafeCoerce (extract :: forall x. f a b x -> x)

instance (Eval expr f, Comonad (f a b c)) => Comonad (ValueOf expr a b c) where
  extract = unsafeCoerce (extract :: forall x. f a b c x -> x)

instance (Eval expr f, Comonad (f a b c d)) => Comonad (ValueOf expr a b c d) where
  extract = unsafeCoerce (extract :: forall x. f a b c d x -> x)

instance (Eval expr f, Comonad (f a b c d e)) => Comonad (ValueOf expr a b c d e) where
  extract = unsafeCoerce (extract :: forall x. f a b c d e x -> x)

instance (Eval expr f, Comonad (f a b c d e g)) => Comonad (ValueOf expr a b c d e g) where
  extract = unsafeCoerce (extract :: forall x. f a b c d e g x -> x)

instance (Eval expr f, Contravariant f) => Contravariant (ValueOf expr) where
  cmap = unsafeCoerce (cmap :: forall a b. (b -> a) -> f a -> f b)

instance (Eval expr f, Contravariant (f a)) => Contravariant (ValueOf expr a) where
  cmap = unsafeCoerce (cmap :: forall x y. (y -> x) -> f a x -> f a y)

instance (Eval expr f, Contravariant (f a b)) => Contravariant (ValueOf expr a b) where
  cmap = unsafeCoerce (cmap :: forall x y. (y -> x) -> f a b x -> f a b y)

instance (Eval expr f, Contravariant (f a b c)) => Contravariant (ValueOf expr a b c) where
  cmap = unsafeCoerce (cmap :: forall x y. (y -> x) -> f a b c x -> f a b c y)

instance (Eval expr f, Contravariant (f a b c d)) => Contravariant (ValueOf expr a b c d) where
  cmap = unsafeCoerce (cmap :: forall x y. (y -> x) -> f a b c d x -> f a b c d y)

instance (Eval expr f, Contravariant (f a b c d e)) => Contravariant (ValueOf expr a b c d e) where
  cmap = unsafeCoerce (cmap :: forall x y. (y -> x) -> f a b c d e x -> f a b c d e y)

instance (Eval expr f, Contravariant (f a b c d e g)) => Contravariant (ValueOf expr a b c d e g) where
  cmap = unsafeCoerce (cmap :: forall x y. (y -> x) -> f a b c d e g x -> f a b c d e g y)

instance (Eval expr p, Semigroupoid p) => Semigroupoid (ValueOf expr) where
  compose = unsafeCoerce (compose :: forall a b c. p b c -> p a b -> p a c)

instance (Eval expr p, Semigroupoid (p e)) => Semigroupoid (ValueOf expr e) where
  compose = unsafeCoerce (compose :: forall a b c. p e b c -> p e a b -> p e a c)

instance (Eval expr p, Semigroupoid (p e g)) => Semigroupoid (ValueOf expr e g) where
  compose = unsafeCoerce (compose :: forall a b c. p e g b c -> p e g a b -> p e g a c)

instance (Eval expr p, Semigroupoid (p e g h)) => Semigroupoid (ValueOf expr e g h) where
  compose = unsafeCoerce (compose :: forall a b c. p e g h b c -> p e g h a b -> p e g h a c)

instance (Eval expr p, Semigroupoid (p e g h i)) => Semigroupoid (ValueOf expr e g h i) where
  compose = unsafeCoerce (compose :: forall a b c. p e g h i b c -> p e g h i a b -> p e g h i a c)

instance (Eval expr p, Semigroupoid (p e g h i j)) => Semigroupoid (ValueOf expr e g h i j) where
  compose = unsafeCoerce (compose :: forall a b c. p e g h i j b c -> p e g h i j a b -> p e g h i j a c)

instance (Eval expr p, Category p) => Category (ValueOf expr) where
  identity = unsafeCoerce (identity :: forall a. p a a)

instance (Eval expr p, Category (p e)) => Category (ValueOf expr e) where
  identity = unsafeCoerce (identity :: forall a. p e a a)

instance (Eval expr p, Category (p e g)) => Category (ValueOf expr e g) where
  identity = unsafeCoerce (identity :: forall a. p e g a a)

instance (Eval expr p, Category (p e g h)) => Category (ValueOf expr e g h) where
  identity = unsafeCoerce (identity :: forall a. p e g h a a)

instance (Eval expr p, Category (p e g h i)) => Category (ValueOf expr e g h i) where
  identity = unsafeCoerce (identity :: forall a. p e g h i a a)

instance (Eval expr p, Category (p e g h i j)) => Category (ValueOf expr e g h i j) where
  identity = unsafeCoerce (identity :: forall a. p e g h i j a a)

instance (Eval expr p, Bifunctor p) => Bifunctor (ValueOf expr) where
  bimap = unsafeCoerce (bimap :: forall a b c d. (a -> b) -> (c -> d) -> p a c -> p b d)

instance (Eval expr p, Bifunctor (p e)) => Bifunctor (ValueOf expr e) where
  bimap = unsafeCoerce (bimap :: forall a b c d. (a -> b) -> (c -> d) -> p e a c -> p e b d)

instance (Eval expr p, Bifunctor (p e g)) => Bifunctor (ValueOf expr e g) where
  bimap = unsafeCoerce (bimap :: forall a b c d. (a -> b) -> (c -> d) -> p e g a c -> p e g b d)

instance (Eval expr p, Bifunctor (p e g h)) => Bifunctor (ValueOf expr e g h) where
  bimap = unsafeCoerce (bimap :: forall a b c d. (a -> b) -> (c -> d) -> p e g h a c -> p e g h b d)

instance (Eval expr p, Bifunctor (p e g h i)) => Bifunctor (ValueOf expr e g h i) where
  bimap = unsafeCoerce (bimap :: forall a b c d. (a -> b) -> (c -> d) -> p e g h i a c -> p e g h i b d)

instance (Eval expr p, Bifunctor (p e g h i j)) => Bifunctor (ValueOf expr e g h i j) where
  bimap = unsafeCoerce (bimap :: forall a b c d. (a -> b) -> (c -> d) -> p e g h i j a c -> p e g h i j b d)

instance (Eval expr p, Biapply p) => Biapply (ValueOf expr) where
  biapply = unsafeCoerce (biapply :: forall a b c d. p (a -> b) (c -> d) -> p a c -> p b d)

instance (Eval expr p, Biapply (p e)) => Biapply (ValueOf expr e) where
  biapply = unsafeCoerce (biapply :: forall a b c d. p e (a -> b) (c -> d) -> p e a c -> p e b d)

instance (Eval expr p, Biapply (p e g)) => Biapply (ValueOf expr e g) where
  biapply = unsafeCoerce (biapply :: forall a b c d. p e g (a -> b) (c -> d) -> p e g a c -> p e g b d)

instance (Eval expr p, Biapply (p e g h)) => Biapply (ValueOf expr e g h) where
  biapply = unsafeCoerce (biapply :: forall a b c d. p e g h (a -> b) (c -> d) -> p e g h a c -> p e g h b d)

instance (Eval expr p, Biapply (p e g h i)) => Biapply (ValueOf expr e g h i) where
  biapply = unsafeCoerce (biapply :: forall a b c d. p e g h i (a -> b) (c -> d) -> p e g h i a c -> p e g h i b d)

instance (Eval expr p, Biapply (p e g h i j)) => Biapply (ValueOf expr e g h i j) where
  biapply = unsafeCoerce (biapply :: forall a b c d. p e g h i j (a -> b) (c -> d) -> p e g h i j a c -> p e g h i j b d)

instance (Eval expr p, Biapplicative p) => Biapplicative (ValueOf expr) where
  bipure = unsafeCoerce (bipure :: forall a b. a -> b -> p a b)

instance (Eval expr p, Biapplicative (p e)) => Biapplicative (ValueOf expr e) where
  bipure = unsafeCoerce (bipure :: forall a b. a -> b -> p e a b)

instance (Eval expr p, Biapplicative (p e g)) => Biapplicative (ValueOf expr e g) where
  bipure = unsafeCoerce (bipure :: forall a b. a -> b -> p e g a b)

instance (Eval expr p, Biapplicative (p e g h)) => Biapplicative (ValueOf expr e g h) where
  bipure = unsafeCoerce (bipure :: forall a b. a -> b -> p e g h a b)

instance (Eval expr p, Biapplicative (p e g h i)) => Biapplicative (ValueOf expr e g h i) where
  bipure = unsafeCoerce (bipure :: forall a b. a -> b -> p e g h i a b)

instance (Eval expr p, Biapplicative (p e g h i j)) => Biapplicative (ValueOf expr e g h i j) where
  bipure = unsafeCoerce (bipure :: forall a b. a -> b -> p e g h i j a b)

instance (Eval expr p, Profunctor p) => Profunctor (ValueOf expr) where
  dimap = unsafeCoerce (dimap :: forall a b c d. (a -> b) -> (c -> d) -> p b c -> p a d)

instance (Eval expr p, Profunctor (p e)) => Profunctor (ValueOf expr e) where
  dimap = unsafeCoerce (dimap :: forall a b c d. (a -> b) -> (c -> d) -> p e b c -> p e a d)

instance (Eval expr p, Profunctor (p e g)) => Profunctor (ValueOf expr e g) where
  dimap = unsafeCoerce (dimap :: forall a b c d. (a -> b) -> (c -> d) -> p e g b c -> p e g a d)

instance (Eval expr p, Profunctor (p e g h)) => Profunctor (ValueOf expr e g h) where
  dimap = unsafeCoerce (dimap :: forall a b c d. (a -> b) -> (c -> d) -> p e g h b c -> p e g h a d)

instance (Eval expr p, Profunctor (p e g h i)) => Profunctor (ValueOf expr e g h i) where
  dimap = unsafeCoerce (dimap :: forall a b c d. (a -> b) -> (c -> d) -> p e g h i b c -> p e g h i a d)

instance (Eval expr p, Profunctor (p e g h i j)) => Profunctor (ValueOf expr e g h i j) where
  dimap = unsafeCoerce (dimap :: forall a b c d. (a -> b) -> (c -> d) -> p e g h i j b c -> p e g h i j a d)

instance (Eval expr p, Strong p) => Strong (ValueOf expr) where
  first = unsafeCoerce (first :: forall a b c. p a b -> p (Tuple a c) (Tuple b c))
  second = unsafeCoerce (second :: forall a b c. p b c -> p (Tuple a b) (Tuple a c))

instance (Eval expr p, Strong (p e)) => Strong (ValueOf expr e) where
  first = unsafeCoerce (first :: forall a b c. p e a b -> p e (Tuple a c) (Tuple b c))
  second = unsafeCoerce (second :: forall a b c. p e b c -> p e (Tuple a b) (Tuple a c))

instance (Eval expr p, Strong (p e g)) => Strong (ValueOf expr e g) where
  first = unsafeCoerce (first :: forall a b c. p e g a b -> p e g (Tuple a c) (Tuple b c))
  second = unsafeCoerce (second :: forall a b c. p e g b c -> p e g (Tuple a b) (Tuple a c))

instance (Eval expr p, Strong (p e g h)) => Strong (ValueOf expr e g h) where
  first = unsafeCoerce (first :: forall a b c. p e g h a b -> p e g h (Tuple a c) (Tuple b c))
  second = unsafeCoerce (second :: forall a b c. p e g h b c -> p e g h (Tuple a b) (Tuple a c))

instance (Eval expr p, Strong (p e g h i)) => Strong (ValueOf expr e g h i) where
  first = unsafeCoerce (first :: forall a b c. p e g h i a b -> p e g h i (Tuple a c) (Tuple b c))
  second = unsafeCoerce (second :: forall a b c. p e g h i b c -> p e g h i (Tuple a b) (Tuple a c))

instance (Eval expr p, Strong (p e g h i j)) => Strong (ValueOf expr e g h i j) where
  first = unsafeCoerce (first :: forall a b c. p e g h i j a b -> p e g h i j (Tuple a c) (Tuple b c))
  second = unsafeCoerce (second :: forall a b c. p e g h i j b c -> p e g h i j (Tuple a b) (Tuple a c))

instance (Eval expr p, Choice p) => Choice (ValueOf expr) where
  right = unsafeCoerce (right :: forall a b c. p b c -> p (Either a b) (Either a c))
  left = unsafeCoerce (left :: forall a b c. p a b -> p (Either a c) (Either b c))

instance (Eval expr p, Choice (p e)) => Choice (ValueOf expr e) where
  right = unsafeCoerce (right :: forall a b c. p e b c -> p e (Either a b) (Either a c))
  left = unsafeCoerce (left :: forall a b c. p e a b -> p e (Either a c) (Either b c))

instance (Eval expr p, Choice (p e g)) => Choice (ValueOf expr e g) where
  right = unsafeCoerce (right :: forall a b c. p e g b c -> p e g (Either a b) (Either a c))
  left = unsafeCoerce (left :: forall a b c. p e g a b -> p e g (Either a c) (Either b c))

instance (Eval expr p, Choice (p e g h)) => Choice (ValueOf expr e g h) where
  right = unsafeCoerce (right :: forall a b c. p e g h b c -> p e g h (Either a b) (Either a c))
  left = unsafeCoerce (left :: forall a b c. p e g h a b -> p e g h (Either a c) (Either b c))

instance (Eval expr p, Choice (p e g h i)) => Choice (ValueOf expr e g h i) where
  right = unsafeCoerce (right :: forall a b c. p e g h i b c -> p e g h i (Either a b) (Either a c))
  left = unsafeCoerce (left :: forall a b c. p e g h i a b -> p e g h i (Either a c) (Either b c))

instance (Eval expr p, Choice (p e g h i j)) => Choice (ValueOf expr e g h i j) where
  right = unsafeCoerce (right :: forall a b c. p e g h i j b c -> p e g h i j (Either a b) (Either a c))
  left = unsafeCoerce (left :: forall a b c. p e g h i j a b -> p e g h i j (Either a c) (Either b c))

instance (Eval expr p, Closed p) => Closed (ValueOf expr) where
  closed = unsafeCoerce (closed :: forall a b x. p a b -> p (x -> a) (x -> b))

instance (Eval expr p, Closed (p e)) => Closed (ValueOf expr e) where
  closed = unsafeCoerce (closed :: forall a b x. p e a b -> p e (x -> a) (x -> b))

instance (Eval expr p, Closed (p e g)) => Closed (ValueOf expr e g) where
  closed = unsafeCoerce (closed :: forall a b x. p e g a b -> p e g (x -> a) (x -> b))

instance (Eval expr p, Closed (p e g h)) => Closed (ValueOf expr e g h) where
  closed = unsafeCoerce (closed :: forall a b x. p e g h a b -> p e g h (x -> a) (x -> b))

instance (Eval expr p, Closed (p e g h i)) => Closed (ValueOf expr e g h i) where
  closed = unsafeCoerce (closed :: forall a b x. p e g h i a b -> p e g h i (x -> a) (x -> b))

instance (Eval expr p, Closed (p e g h i j)) => Closed (ValueOf expr e g h i j) where
  closed = unsafeCoerce (closed :: forall a b x. p e g h i j a b -> p e g h i j (x -> a) (x -> b))
