module FreeSemigroupoid.Functor
  ( FreeFunctor
  , lift
  , hoist
  , lower
  , run
  ) where

import Prelude

import FreeSemigroupoid.Category (FreeCategory)
import FreeSemigroupoid.Category as FreeCategory
import FreeSemigroupoid.Equality as Equality
import FreeSemigroupoid.Function as FreeFunction
import FreeSemigroupoid.Unsafe (UnsafeExists)
import Unsafe.Coerce (unsafeCoerce)

data FreeFunctor f a = FreeFunctor (FreeCategory (->) UnsafeExists a) (f UnsafeExists)

instance functorFreeFunctor :: Functor (FreeFunctor f) where
  map k (FreeFunctor ks a) = FreeFunctor (FreeCategory.lift k <<< ks) a

lift :: forall f a. f a -> FreeFunctor f a
lift b = FreeFunctor (unsafeCoerce (identity :: FreeCategory (->) a a)) (unsafeCoerce b)

hoist :: forall f g. (f ~> g) -> FreeFunctor f ~> FreeFunctor g
hoist k (FreeFunctor ks a) = FreeFunctor ks (k a)

lower :: forall f a. Functor f => FreeFunctor f a -> f a
lower = run \c f ->
  FreeCategory.run
    (flip Equality.coerce1 f)
    (flip map f <<< FreeFunction.run)
    c

run
  :: forall f a r
   . (forall x. FreeCategory (->) x a -> f x -> r)
  -> FreeFunctor f a
  -> r
run k (FreeFunctor ks a) = k ks a
