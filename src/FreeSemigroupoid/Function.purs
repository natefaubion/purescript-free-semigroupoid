module FreeSemigroupoid.Function where

import FreeSemigroupoid (FreeSemigroupoid)
import FreeSemigroupoid as FreeSemigroupoid
import FreeSemigroupoid.Unsafe (unsafeRunExists)

run :: forall a b. FreeSemigroupoid (->) a b -> a -> b
run = go
  where
  go :: forall x. FreeSemigroupoid (->) x b -> x -> b
  go fn acc = case unsafeRunExists (FreeSemigroupoid.unconsView fn) of
    FreeSemigroupoid.UnconsDone k -> k acc
    FreeSemigroupoid.UnconsMore k more -> go more (k acc)
