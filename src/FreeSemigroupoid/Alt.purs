module FreeSemigroupoid.Alt
  ( FreeAlt
  , FreeAltView(..)
  , lift
  , hoist
  , promote
  , demote
  , lower
  , view
  , run
  ) where

import Prelude

import Control.Alt (class Alt)
import Data.Exists (Exists)
import Data.Newtype (un)
import FreeSemigroupoid (FreeSemigroupoid)
import FreeSemigroupoid as FreeSemigroupoid
import FreeSemigroupoid.Category as FreeCategory
import FreeSemigroupoid.Equality as Equality
import FreeSemigroupoid.Function as FreeFunction
import FreeSemigroupoid.Functor (FreeFunctor)
import FreeSemigroupoid.Functor as FreeFunctor
import FreeSemigroupoid.Phantom1 (Phantom1(..))
import FreeSemigroupoid.Phantom1 as Phantom1
import Unsafe.Coerce (unsafeCoerce)

newtype FreeAlt f a = FreeAlt (forall x y. FreeSemigroupoid (Phantom1 (FreeFunctor f) a) x y)

instance functorFreeAlt :: Functor (FreeAlt f) where
  map k (FreeAlt f) = FreeAlt (FreeSemigroupoid.hoist (Phantom1.over k) f)

instance altFreeAlt :: Alt (FreeAlt f) where
  alt (FreeAlt a) (FreeAlt b) = FreeAlt (b <<< a)

lift :: forall f a. f a -> FreeAlt f a
lift f = FreeAlt (FreeSemigroupoid.lift (Phantom1 (FreeFunctor.lift f)))

hoist :: forall f g. (f ~> g) -> FreeAlt f ~> FreeAlt g
hoist k (FreeAlt f) = FreeAlt (FreeSemigroupoid.hoist (Phantom1.hoist (FreeFunctor.hoist k)) f)

promote :: forall f a x y. FreeAlt f a -> FreeSemigroupoid (Phantom1 (FreeFunctor f) a) x y
promote (FreeAlt f) = f

demote :: forall f a x y. FreeSemigroupoid (Phantom1 (FreeFunctor f) a) x y -> FreeAlt f a
demote = FreeAlt <<< Phantom1.recompose

lower :: forall f a. Alt f => FreeAlt f a -> f a
lower = un Phantom1 <<< FreeSemigroupoid.lower <<< FreeSemigroupoid.hoist (Phantom1.hoist FreeFunctor.lower) <<< promote

data FreeAltView f a x
  = AltMore (f a) (FreeAlt f a)
  | AltMapMore (x -> a) (f x) (FreeAlt f a)
  | AltMap (x -> a) (f x)
  | AltDone (f a)

view :: forall f a. FreeAlt f a -> Exists (FreeAltView f a)
view = run (unsafeCoerce AltMore) (unsafeCoerce AltMapMore) (unsafeCoerce AltMap) (unsafeCoerce AltDone)

run
  :: forall f a r
   . (f a -> FreeAlt f a -> r)
  -> (forall x. (x -> a) -> f x -> FreeAlt f a -> r)
  -> (forall x. (x -> a) -> f x -> r)
  -> (f a -> r)
  -> FreeAlt f a
  -> r
run kmore kmapmore kmap kdone = \(FreeAlt fa) -> go fa
  where
  go = FreeSemigroupoid.uncons
    (\(Phantom1 ks) more ->
      FreeFunctor.run
        (\k f ->
            FreeCategory.run
              (\e -> kdone (Equality.coerce1 e f))
              (\j -> kmapmore (FreeFunction.run j) f (demote more))
              k)
        ks)
    (\(Phantom1 ks) ->
      FreeFunctor.run
        (\k f ->
          FreeCategory.run
            (\e -> kdone (Equality.coerce1 e f))
            (\j -> kmap (FreeFunction.run j) f)
            k
        )
        ks)
