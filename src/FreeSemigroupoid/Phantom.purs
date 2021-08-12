module FreeSemigroupoid.Phantom where

import Prelude

import Data.Newtype (class Newtype)
import FreeSemigroupoid (FreeSemigroupoid)
import FreeSemigroupoid as FreeSemigroupoid
import Unsafe.Coerce (unsafeCoerce)

newtype Phantom f a b = Phantom f

hoist :: forall f g a b c d. (f -> g) -> Phantom f a b -> Phantom g c d
hoist f (Phantom a) = Phantom (f a)

promote :: forall f a b c d. Phantom f a b -> FreeSemigroupoid (Phantom f) c d
promote = FreeSemigroupoid.lift <<< hoist identity

derive instance newtypePhantom :: Newtype (Phantom f a b) _

instance semigroupoidPhantom :: Semigroup f => Semigroupoid (Phantom f) where
  compose (Phantom a) (Phantom b) = Phantom (a <> b)

recomposeProof :: forall f a b c d. FreeSemigroupoid (Phantom f) a b -> FreeSemigroupoid (Phantom f) c d
recomposeProof = FreeSemigroupoid.uncons (\a b -> recomposeProof b<<< promote a) promote

recompose :: forall f a b c d. FreeSemigroupoid (Phantom f) a b -> FreeSemigroupoid (Phantom f) c d
recompose = unsafeCoerce
