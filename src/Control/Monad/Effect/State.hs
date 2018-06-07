{-# LANGUAGE DataKinds, KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}

{-|
Module      : Control.Monad.Effect.State
Description : State effects for computations that carry state
Copyright   : Allele Dev 2015
License     : BSD-3
Maintainer  : allele.dev@gmail.com
Stability   : experimental
Portability : POSIX

Composable handler for State effects.

Using <http://okmij.org/ftp/Haskell/extensible/Eff1.hs> as a
starting point.

-}
module Control.Monad.Effect.State (
  State(..),
  get,
  gets,
  put,
  modify,
  modify',
  runState,
  localState,
  transactionState
) where

import Control.Monad.Effect.Internal
import Data.Proxy

--------------------------------------------------------------------------------
                         -- State, strict --
--------------------------------------------------------------------------------

-- | Run a 'State s' effect given an effect and an initial state.
runState :: Effectful m => s -> m (State s ': e) b -> m e (b, s)
runState initial = relayState initial (\ s b -> raiseEff (pure (b, s))) (\ s eff yield -> case eff of
  Get    -> yield s s
  Put s' -> yield s' ())


-- | Strict State effects: one can either Get values or Put them
data State s (m :: * -> *) v where
  Get :: State s m s
  Put :: !s -> State s m ()

-- | Retrieve state
get :: (Member (State s) e, Effectful m) => m e s
get = send Get

-- | Retrieve state, modulo a projection.
gets :: (Member (State s) e, Effectful m) => (s -> a) -> m e a
gets f = raiseEff (f <$> get)

-- | Store state
put :: (Member (State s) e, Effectful m) => s -> m e ()
put s = send (Put s)

-- | Modify state
modify :: (Member (State s) e, Effectful m) => (s -> s) -> m e ()
modify f = raiseEff (fmap f get >>= put)

-- | Modify state strictly
modify' :: (Member (State s) e, Effectful m) => (s -> s) -> m e ()
modify' f = raiseEff $ do
  v <- get
  put $! f v

-- |
-- An encapsulated State handler, for transactional semantics
-- The global state is updated only if the transactionState finished
-- successfully
transactionState :: forall s e a m. (Member (State s) e, Effectful m)
                    => Proxy s
                    -> m e a
                    -> m e a
transactionState _ m = raiseEff $ do s <- get; loop s (lowerEff m)
 where
   loop :: s -> Eff e a -> Eff e a
   loop s (Val x) = put s >> pure x
   loop s (E (u :: Union e Identity b) q) = case prj u :: Maybe (State s Identity b) of
     Just Get      -> loop s (apply q s)
     Just (Put s') -> loop s'(apply q ())
     _             -> E u (tsingleton k)
      where k = q >>> (loop s)

localState :: (Member (State s) effects, Effectful m) => (s -> s) -> m effects a -> m effects a
localState f action = raiseEff $ do
  original <- get
  put (f original)
  v <- lowerEff action
  put original
  pure v
