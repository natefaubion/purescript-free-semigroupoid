module FreeSemigroupoid.Category
  ( FreeCategory
  , lift
  , hoist
  , lower
  , run
  ) where

import Prelude

import FreeSemigroupoid (FreeSemigroupoid)
import FreeSemigroupoid as FreeSemigroupoid
import FreeSemigroupoid.Equality (Equality)
import FreeSemigroupoid.Equality as Equality

data FreeCategory c a b
  = Compose (FreeSemigroupoid c a b)
  | Id (Equality a b)

instance freeCategorySemigroupoid :: Semigroupoid (FreeCategory c) where
  compose = case _, _ of
    Id e, c -> Equality.coerce1 e c
    c, Id e -> Equality.coerce2 (Equality.flip e) c
    Compose c, Compose d -> Compose (c <<< d)

instance freeCategoryCategory :: Category (FreeCategory c) where
  identity = Id Equality.refl

lift :: forall c a b. c a b -> FreeCategory c a b
lift = Compose <<< FreeSemigroupoid.lift

hoist :: forall c d a b. (forall x y. c x y -> d x y) -> FreeCategory c a b -> FreeCategory d a b
hoist k = case _ of
  Id e -> Id e
  Compose f -> Compose (FreeSemigroupoid.hoist k f)

lower :: forall c a b. Category c => FreeCategory c a b -> c a b
lower = run (flip Equality.coerce1 identity) FreeSemigroupoid.lower

run :: forall c a b r. (Equality a b -> r) -> (FreeSemigroupoid c a b -> r) -> FreeCategory c a b -> r
run done more = case _ of
  Id e -> done e
  Compose ks -> more ks
