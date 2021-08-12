module FreeSemigroupoid
  ( FreeSemigroupoid
  , lift
  , hoist
  , lower
  , foldl
  , foldr
  , UnconsView(..)
  , unconsView
  , uncons
  , UnsnocView(..)
  , unsnocView
  , unsnoc
  ) where

import Prelude

import Data.Exists (Exists)
import FreeSemigroupoid.Unsafe (unsafeRunExists)
import Unsafe.Coerce (unsafeCoerce)

foreign import data UnsafeBoundValue :: Type

foreign import data UnsafeBoundSemigroupoid :: Type -> Type -> Type

data FreeSemigroupoid c a b
  = Lift (c a b)
  | Append (FreeSemigroupoid c a UnsafeBoundValue) (FreeSemigroupoid c UnsafeBoundValue b)
  | Hoist (forall x y. UnsafeBoundSemigroupoid x y -> c x y) (FreeSemigroupoid UnsafeBoundSemigroupoid a b)

instance semigroupoidFree :: Semigroupoid (FreeSemigroupoid c) where
  compose a b = Append (unsafeCoerce b) (unsafeCoerce a)

lift :: forall c a b. c a b -> FreeSemigroupoid c a b
lift = Lift

hoist
  :: forall c d a b
   . (forall x y. c x y -> d x y)
  -> FreeSemigroupoid c a b
  -> FreeSemigroupoid d a b
hoist k a = Hoist (unsafeCoerce k) (unsafeCoerce a)

lower
  :: forall c a b
   . Semigroupoid c
  => FreeSemigroupoid c a b
  -> c a b
lower = foldl (>>>) identity

foldl
  :: forall c d a b
   . (forall x y z. d x y -> c y z -> d x z)
  -> (forall x. c a x -> d a x)
  -> FreeSemigroupoid c a b
  -> d a b
foldl kmore kdone = go1
  where
  go1 :: FreeSemigroupoid c a b -> d a b
  go1 f = case unsafeRunExists (unconsView f) of
    UnconsDone c -> kdone c
    UnconsMore c more -> go2 (kdone c) more

  go2 :: forall x y z. d x y -> FreeSemigroupoid c y z -> d x z
  go2 acc f = case unsafeRunExists (unconsView f) of
    UnconsDone c -> kmore acc c
    UnconsMore c more -> go2 (kmore acc c) more

foldr
  :: forall c d a b
   . (forall x y z. c x y -> d y z -> d x z)
  -> (forall x. c x b -> d x b)
  -> FreeSemigroupoid c a b
  -> d a b
foldr kmore kdone = go1
  where
  go1 :: FreeSemigroupoid c a b -> d a b
  go1 f = case unsafeRunExists (unsnocView f) of
    UnsnocDone c -> kdone c
    UnsnocMore more c -> go2 more (kdone c)

  go2 :: forall x y z. FreeSemigroupoid c x y -> d y z -> d x z
  go2 f acc = case unsafeRunExists (unsnocView f) of
    UnsnocDone c -> kmore c acc
    UnsnocMore more c -> go2 more (kmore c acc)

data UnconsView c a b x
  = UnconsDone (c a b)
  | UnconsMore (c a x) (FreeSemigroupoid c x b)

unconsView :: forall c a b. FreeSemigroupoid c a b -> Exists (UnconsView c a b)
unconsView = uncons (unsafeCoerce UnconsMore) (unsafeCoerce UnconsDone)

uncons
  :: forall c a b r
   . (forall x. c a x -> FreeSemigroupoid c x b -> r)
  -> (c a b -> r)
  -> FreeSemigroupoid c a b
  -> r
uncons more done = case _ of
  Lift a -> done a
  Append a b -> uncons2 more a b
  Hoist k a -> unconsHoist more done k a

unconsHoist
  :: forall c d a b r
   . (forall x. d a x -> FreeSemigroupoid d x b -> r)
  -> (d a b -> r)
  -> (forall y z. c y z -> d y z)
  -> FreeSemigroupoid c a b
  -> r
unconsHoist more done h = case _ of
  Lift a -> done (h a)
  Append a b -> uncons2Hoist more h a b
  Hoist k a -> unconsHoist more done (h <<< k) a

uncons2
  :: forall c a x b r
   . (forall z. c a z -> FreeSemigroupoid c z b -> r)
  -> FreeSemigroupoid c a x
  -> FreeSemigroupoid c x b
  -> r
uncons2 more l r = case l of
  Lift a -> more (unsafeCoerce a) (unsafeCoerce r)
  Append l' r' -> uncons2 more l' (Append (unsafeCoerce r') (unsafeCoerce r))
  Hoist k a -> uncons2Hoist more k a (unsafeCoerce r)

uncons2Hoist
  :: forall c d a x b r
   . (forall z. d a z -> FreeSemigroupoid d z b -> r)
  -> (forall y z. c y z -> d y z)
  -> FreeSemigroupoid c a x
  -> FreeSemigroupoid c x b
  -> r
uncons2Hoist more h l r = case l of
  Lift a -> more (unsafeCoerce (h a)) (Hoist (unsafeCoerce h) (unsafeCoerce r))
  Append l' r' -> uncons2Hoist more h l' (Append (unsafeCoerce r') (unsafeCoerce r))
  Hoist k a -> uncons2Hoist more (h <<< k) a (unsafeCoerce r)

data UnsnocView c a b x
  = UnsnocDone (c a b)
  | UnsnocMore (FreeSemigroupoid c a x) (c x b)

unsnocView :: forall c a b. FreeSemigroupoid c a b -> Exists (UnsnocView c a b)
unsnocView = unsnoc (unsafeCoerce UnsnocMore) (unsafeCoerce UnsnocDone)

unsnoc
  :: forall c a b r
   . (forall x. FreeSemigroupoid c a x -> c x b -> r)
  -> (c a b -> r)
  -> FreeSemigroupoid c a b
  -> r
unsnoc more done = case _ of
  Lift a -> done a
  Append a b -> unsnoc2 more a b
  Hoist k a -> unsnocHoist more done k a

unsnocHoist
  :: forall c d a b r
   . (forall x. FreeSemigroupoid d a x -> d x b -> r)
  -> (d a b -> r)
  -> (forall y z. c y z -> d y z)
  -> FreeSemigroupoid c a b
  -> r
unsnocHoist more done h = case _ of
  Lift a -> done (h a)
  Append a b -> unsnoc2Hoist more h a b
  Hoist k a -> unsnocHoist more done (h <<< k) a

unsnoc2
  :: forall c a x b r
   . (forall z. FreeSemigroupoid c a z -> c z b -> r)
  -> FreeSemigroupoid c a x
  -> FreeSemigroupoid c x b
  -> r
unsnoc2 more l r = case r of
  Lift a -> more (unsafeCoerce l) (unsafeCoerce a)
  Append l' r' -> unsnoc2 more (Append (unsafeCoerce l) (unsafeCoerce r')) r'
  Hoist k a -> unsnoc2Hoist more k (unsafeCoerce l) a

unsnoc2Hoist
  :: forall c d a x b r
   . (forall z. FreeSemigroupoid d a z -> d z b -> r)
  -> (forall y z. c y z -> d y z)
  -> FreeSemigroupoid c a x
  -> FreeSemigroupoid c x b
  -> r
unsnoc2Hoist more h l r = case r of
  Lift a -> more (Hoist (unsafeCoerce h) (unsafeCoerce l)) (unsafeCoerce (h a))
  Append l' r' -> unsnoc2Hoist more h (Append (unsafeCoerce l) (unsafeCoerce l')) r'
  Hoist k a -> unsnoc2Hoist more (h <<< k) (unsafeCoerce l) a
