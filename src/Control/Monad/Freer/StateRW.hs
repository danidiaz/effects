{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

{-|
Module      : Control.Monad.Freer.StateRW
Description : State effects in terms of Reader/Writer
Copyright   : Alej Cabrera 2015
License     : BSD-3
Maintainer  : cpp.cabrera@gmail.com
Stability   : experimental
Portability : POSIX

Composable handler for State effects in terms of Reader/Writer
effects. This module is more a tutorial on how to compose handlers. It
is slightly slower than a dedicated State handler.

Using <http://okmij.org/ftp/Haskell/extensible/Eff1.hs> as a
starting point.

-}
module Control.Monad.Freer.StateRW (
  runStateR,
  Reader,
  Writer,
  tell,
  ask
) where

import Control.Monad.Freer.Reader
import Control.Monad.Freer.Writer
import Control.Monad.Freer.Internal

-- | State handler, using Reader/Writer effects
runStateR :: Eff (Writer s ': Reader s ': effs) a -> s -> Eff effs (a, s)
runStateR m s = loop s m
 where
   loop :: s -> Eff (Writer s ': Reader s ': effs) a -> Eff effs (a, s)
   loop s' (Val x) = return (x,s')
   loop s' (E u q) = case decomp u of
     Right (Writer o) -> k o ()
     Left  u'  -> case decomp u' of
       Right Reader -> k s' s'
       Left u'' -> E u'' (tsingleton (k s'))
    where k s'' = composeEffs q (loop s'')
