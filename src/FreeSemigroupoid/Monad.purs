module FreeSemigroupoid.Monad
  ( FreeMonad
  , FreeMonadView(..)
  , Kleisli(..)
  , lift
  , hoist
  , lower
  , view
  , run
  ) where

import Prelude

import Data.Exists (Exists)
import Data.Newtype (class Newtype)
import Data.Newtype as Newtype
import FreeSemigroupoid (FreeSemigroupoid)
import FreeSemigroupoid as FreeSemigroupoid
import FreeSemigroupoid.Unsafe (UnsafeExists, unsafeRunExists)
import Unsafe.Coerce (unsafeCoerce)

newtype Kleisli f a b = Kleisli (a -> f b)

derive instance newtypeKleisli :: Newtype (Kleisli f a b) _

data FreeMonad f a
  = Bind (FreeMonad f UnsafeExists) (FreeSemigroupoid (Kleisli (FreeMonad f)) UnsafeExists a)
  | Lift (f a)
  | Pure a

instance functorFreeMonad :: Functor (FreeMonad f) where
  map k = case _ of
    Bind f ks -> Bind f (FreeSemigroupoid.lift (Kleisli (Pure <<< k)) <<< ks)
    f -> Bind (unsafeCoerce f) (FreeSemigroupoid.lift (Kleisli (Pure <<< unsafeCoerce k)))

instance applyFreeMonad :: Apply (FreeMonad f) where
  apply = ap

instance applicativeFreeMonad :: Applicative (FreeMonad f) where
  pure = Pure

instance bindFreeMonad :: Bind (FreeMonad f) where
  bind ff k = case ff of
    Bind f ks -> Bind f (FreeSemigroupoid.lift (Kleisli k) <<< ks)
    f -> Bind (unsafeCoerce f) (FreeSemigroupoid.lift (Kleisli (unsafeCoerce k)))

instance monadFreeMonad :: Monad (FreeMonad f)

lift :: forall f. f ~> FreeMonad f
lift = Lift

hoist :: forall f g. (f ~> g) -> FreeMonad f ~> FreeMonad g
hoist k = case _ of
  Bind f ks -> Bind (hoist k f) (FreeSemigroupoid.hoist (Newtype.over Kleisli (compose (hoist k))) ks)
  Lift f -> Lift (k f)
  Pure a -> Pure a

lower :: forall f. Monad f => FreeMonad f ~> f
lower = case _ of
  Bind (Pure a) ks -> go ks a
  Bind f ks -> lower f >>= go ks
  Lift f -> f
  Pure a -> pure a
  where
  go :: forall x y. FreeSemigroupoid (Kleisli (FreeMonad f)) x y -> x -> f y
  go ks a = case unsafeRunExists (FreeSemigroupoid.unconsView ks) of
    FreeSemigroupoid.UnconsMore (Kleisli k) more -> lower (k a) >>= go more
    FreeSemigroupoid.UnconsDone (Kleisli k) -> lower (k a)

data FreeMonadView f a x
  = MonadBind (f x) (x -> FreeMonad f a)
  | MonadLift (f a)
  | MonadPure a

view :: forall f a. FreeMonad f a -> Exists (FreeMonadView f a)
view = run (unsafeCoerce MonadBind) (unsafeCoerce MonadLift) (unsafeCoerce MonadPure)

run
  :: forall f a r
   . (forall x. f x -> (x -> FreeMonad f a) -> r)
  -> (f a -> r)
  -> (a -> r)
  -> FreeMonad f a
  -> r
run kmore klift kpure = go1
  where
  go1 :: FreeMonad f a -> r
  go1 = case _ of
    Bind f ks ->
      case f of
        Bind f' ks' -> go1 (Bind f' (ks <<< ks'))
        Lift f' -> kmore f' (go2 ks)
        Pure a -> go1 (go2 ks a)
    Lift f -> klift f
    Pure a -> kpure a

  go2 :: forall x y. FreeSemigroupoid (Kleisli (FreeMonad f)) x y -> x -> FreeMonad f y
  go2 ks a = case unsafeRunExists (FreeSemigroupoid.unconsView ks) of
    FreeSemigroupoid.UnconsMore (Kleisli k) more -> Bind (k a) more
    FreeSemigroupoid.UnconsDone (Kleisli k) -> k a

