{-# LANGUAGE TypeOperators, TypeApplications, ScopedTypeVariables, GADTs, FlexibleContexts, DataKinds, Rank2Types #-}
module Control.Monad.Effect.Resumable
  ( Resumable(..)
  , SomeExc(..)
  , throwResumable
  , catchResumable
  , runResumable
  , runResumableWith
  ) where

import Data.Functor.Classes
import Control.Monad.Effect.Internal

data Resumable exc a = Resumable (exc a)

throwResumable :: (Member (Resumable exc) e, Effectful m) => exc v -> m e v
throwResumable = send . Resumable

catchResumable :: forall exc e m a. (Member (Resumable exc) e, Effectful m)
               => m e a
               -> (forall v. exc v -> m e v)
               -> m e a
catchResumable m handle = raiseHandler (interpose @(Resumable exc) pure (\(Resumable e) yield -> lowerEff (handle e) >>= yield)) m


runResumable :: Effectful m => m (Resumable exc ': e) a -> m e (Either (SomeExc exc) a)
runResumable = raiseHandler (relay (pure . Right) (\ (Resumable e) _ -> pure (Left (SomeExc e))))

-- | Run a 'Resumable' effect in an 'Effectful' context, using a handler to resume computation.
runResumableWith :: Effectful m => (forall resume . exc resume -> m effects resume) -> m (Resumable exc ': effects) a -> m effects a
runResumableWith handler = raiseHandler (relay pure (\ (Resumable err) yield -> lowerEff (handler err) >>= yield))

data SomeExc exc where
  SomeExc :: exc v -> SomeExc exc

instance Eq1 exc => Eq (SomeExc exc) where
  SomeExc exc1 == SomeExc exc2 = liftEq (const (const True)) exc1 exc2

instance (Show1 exc) => Show (SomeExc exc) where
  showsPrec num (SomeExc exc) = liftShowsPrec (const (const id)) (const id) num exc
