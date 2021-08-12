module FreeSemigroupoid.Equality
  ( Equality
  , refl
  , flip
  , coerce
  , coerce1
  , coerce2
  ) where

import Prelude

import Unsafe.Coerce (unsafeCoerce)

newtype Equality a b = Equality (forall f. f a -> f b)

instance semigroupoidEquality :: Semigroupoid Equality where
  compose _ _ = Equality unsafeCoerce

instance categoryEquality :: Category Equality where
  identity = refl

refl :: forall a. Equality a a
refl = Equality \a -> a

flip :: forall a b. Equality a b -> Equality b a
flip = unsafeCoerce

coerce :: forall a b. Equality a b -> a -> b
coerce _ = unsafeCoerce

coerce1 :: forall f a b. Equality a b -> f a -> f b
coerce1 _ = unsafeCoerce

coerce2 :: forall f a b c. Equality a b -> f a c -> f b c
coerce2 _ = unsafeCoerce
