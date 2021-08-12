module FreeSemigroupoid.Semigroup where

import Prelude

import Data.Foldable (class Foldable, foldlDefault, foldrDefault)
import Data.Newtype (un)
import Data.Semigroup.Foldable (class Foldable1, foldl1Default, foldr1Default)
import FreeSemigroupoid (FreeSemigroupoid)
import FreeSemigroupoid as FreeSemigroupoid
import FreeSemigroupoid.Phantom (Phantom(..))
import FreeSemigroupoid.Phantom as Phantom

newtype FreeSemigroup a = FreeSemigroup (forall x y. FreeSemigroupoid (Phantom a) x y)

promote :: forall a x y. FreeSemigroup a -> FreeSemigroupoid (Phantom a) x y
promote (FreeSemigroup a) = a

instance functorFreeSemigroup :: Functor FreeSemigroup where
  map f (FreeSemigroup c) = FreeSemigroup (FreeSemigroupoid.hoist (Phantom.hoist f) c)

instance semigroupFreeSemigroup :: Semigroup (FreeSemigroup a) where
  append (FreeSemigroup a) (FreeSemigroup b) = FreeSemigroup (b <<< a)

instance foldableFreeSemigroup :: Foldable FreeSemigroup where
  foldMap k = lower <<< map k
  foldr a = foldrDefault a
  foldl a = foldlDefault a

instance foldable1 :: Foldable1 FreeSemigroup where
  foldMap1 k = lower <<< map k
  foldr1 a = foldr1Default a
  foldl1 a = foldl1Default a

lift :: forall a. a -> FreeSemigroup a
lift a = FreeSemigroup (FreeSemigroupoid.lift (Phantom a))

lower :: forall a. Semigroup a => FreeSemigroup a -> a
lower = un Phantom <<< FreeSemigroupoid.lower <<< promote
