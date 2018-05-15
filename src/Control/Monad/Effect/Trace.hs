{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

{-|
Module      : Control.Monad.Effect.Trace
Description : Composable Trace effects
Copyright   : Allele Dev 2015
License     : BSD-3
Maintainer  : allele.dev@gmail.com
Stability   : experimental
Portability : POSIX

Composable handler for Trace effects. Trace allows one to debug the
operation of sequences of effects by outputing to the console.

Using <http://okmij.org/ftp/Haskell/extensible/Eff1.hs> as a
starting point.

-}
module Control.Monad.Effect.Trace
( Trace(..)
, trace
, runPrintingTrace
, runIgnoringTrace
, runReturningTrace
) where

import Control.Monad.Effect.Internal
import Control.Monad.Effect.State
import System.IO

-- | A Trace effect; takes a String and performs output
data Trace v where
  Trace :: String -> Trace ()

-- | Printing a string in a trace
trace :: (Member Trace e, Effectful m) => String -> m e ()
trace = send . Trace

-- | An IO handler for Trace effects. Prints output to stderr.
runPrintingTrace :: (Member IO effects, Effectful m) => m (Trace ': effects) a -> m effects a
runPrintingTrace = raiseHandler (relay pure (\ (Trace s) -> (send (hPutStrLn stderr s) >>=)))

-- | Run a 'Trace' effect, discarding the traced values.
runIgnoringTrace :: Effectful m => m (Trace ': effects) a -> m effects a
runIgnoringTrace = raiseHandler (interpret (\ (Trace _) -> pure ()))

-- | Run a 'Trace' effect, accumulating the traced values into a list like a 'Writer'.
runReturningTrace :: Effectful m => m (Trace ': effects) a -> m effects (a, [String])
runReturningTrace = raiseHandler (fmap (fmap reverse) . runState [] . reinterpret (\ (Trace s) -> modify' (s:)))
