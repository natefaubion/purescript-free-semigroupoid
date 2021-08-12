module FreeSemigroupoid.Phantom1 where

import Prelude

import Control.Alt (class Alt, (<|>))
import Data.Newtype (class Newtype)
import FreeSemigroupoid (FreeSemigroupoid)
import FreeSemigroupoid as FreeSemigroupoid
import Unsafe.Coerce (unsafeCoerce)

newtype Phantom1 f x a b = Phantom1 (f x)

hoist :: forall f g x a b c d. (f ~> g) -> Phantom1 f x a b -> Phantom1 g x c d
hoist f (Phantom1 a) = Phantom1 (f a)

over :: forall f x y a b c d. Functor f => (x -> y) -> Phantom1 f x a b -> Phantom1 f y c d
over f (Phantom1 a) = Phantom1 (f <$> a)

promote :: forall f x a b c d. Phantom1 f x a b -> FreeSemigroupoid (Phantom1 f x) c d
promote = FreeSemigroupoid.lift <<< hoist identity

derive instance newtypePhantom1 :: Newtype (Phantom1 f x a b) _

instance semigroupoidPhantom1 :: Alt f => Semigroupoid (Phantom1 f a) where
  compose (Phantom1 a) (Phantom1 b) = Phantom1 (a <|> b)

recomposeProof :: forall f x a b c d. FreeSemigroupoid (Phantom1 f x) a b -> FreeSemigroupoid (Phantom1 f x) c d
recomposeProof = FreeSemigroupoid.uncons (\a b -> recomposeProof b <<< promote a) promote

recompose :: forall f x a b c d. FreeSemigroupoid (Phantom1 f x) a b -> FreeSemigroupoid (Phantom1 f x) c d
recompose = unsafeCoerce
