{-# LANGUAGE DataKinds, FlexibleContexts, GADTs, TypeApplications, TypeOperators, UndecidableInstances #-}

{-|
Module      : Control.Monad.Effect.NonDet
Description : Nondeterministic Choice effects
Copyright   : Allele Dev 2015
License     : BSD-3
Maintainer  : allele.dev@gmail.com
Stability   : experimental
Portability : POSIX
-}


module Control.Monad.Effect.NonDet (
  NonDet(..),
  runNonDetM,
  gatherM,
  runNonDetA,
  runNonDet,
  msplit
) where

import Control.Applicative
import Control.Monad
import Control.Monad.Effect.Internal
import Data.Foldable (asum)

--------------------------------------------------------------------------------
                    -- Nondeterministic Choice --
--------------------------------------------------------------------------------

runNonDetM :: (Monoid b, Effectful m, Effect (Union e))
           => (a -> b)
           -> m (NonDet ': e) a
           -> m e b
runNonDetM unit = raiseHandler (fmap (foldMap unit) . runNonDet)

gatherM :: (Monoid b, Member NonDet e, Effectful m)
        => (a -> b) -- ^ A function constructing a 'Monoid'al value from a single computed result. This might typically be @unit@ (for @Reducer@s), 'pure' (for 'Applicative's), or some similar singleton constructor.
        -> m e a    -- ^ The computation to run locally-nondeterministically.
        -> m e b
gatherM f = raiseHandler (interpose (pure . f) (\ m k -> case m of
  MZero -> pure mempty
  MPlus -> mappend <$> k True <*> k False))

-- | A handler for nondeterminstic effects
runNonDetA :: (Alternative f, Effectful m, Effect (Union e))
           => m (NonDet ': e) a
           -> m e (f a)
runNonDetA = raiseHandler (fmap (asum . map pure) . runNonDet)

-- | A handler for nondeterminstic effects
runNonDet :: (Effectful m, Effect (Union e))
           => m (NonDet ': e) a
           -> m e [a]
runNonDet = raiseHandler go
  where go (Return a)       = pure [a]
        go (Effect MZero _) = pure []
        go (Effect MPlus k) = liftA2 (++) (runNonDetA (k True)) (runNonDetA (k False))
        go (Other u k)      = handleStateful [] (fmap join . traverse runNonDetA) u k

-- FIXME: It would probably be more efficient to define these in terms of a binary tree rather than a list.

msplit :: (Member NonDet e, Effectful m)
       => m e a -> m e (Maybe (a, m e a))
msplit = raiseHandler (fmap (fmap (fmap raiseEff)) . loop [])
  where loop jq (Return x) = pure (Just (x, msum jq))
        loop jq (E u q) =
          case prj u of
            Just MZero ->
              case jq of
                []      -> pure Nothing
                (j:jq') -> loop jq' j
            Just MPlus  -> loop (q `apply` False : jq) (q `apply` True)
            Nothing     -> E u (tsingleton k)
              where k = q >>> loop jq
