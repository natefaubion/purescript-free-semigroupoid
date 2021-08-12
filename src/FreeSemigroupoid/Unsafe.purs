module FreeSemigroupoid.Unsafe where

import Data.Exists (Exists)
import Unsafe.Coerce (unsafeCoerce)

foreign import data UnsafeExists :: Type

foreign import data UnsafeExistsSemigroupoid :: Type -> Type -> Type

unsafeRunExists :: forall f. Exists f -> f UnsafeExists
unsafeRunExists = unsafeCoerce

unsafePack1 :: forall f a. f a -> f UnsafeExists
unsafePack1 = unsafeCoerce

unsafePack2 :: forall f a b. f a b -> f UnsafeExists b
unsafePack2 = unsafeCoerce

unsafePack3 :: forall f a b. f a b -> UnsafeExistsSemigroupoid a b
unsafePack3 = unsafeCoerce
