{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

{-|
Module      : Control.Monad.Effect.Cut
Description : An implementation of logical Cut
Copyright   : Allele Dev 2015
License     : BSD-3
Maintainer  : allele.dev@gmail.com
Stability   : broken
Portability : POSIX

Composable handler for logical Cut effects. Implemented in terms of
Exc effect.

Using <http://okmij.org/ftp/Haskell/extensible/Eff1.hs> as a
starting point.

-}
module Control.Monad.Effect.Cut (
  CutFalse,
  cutFalse,
  -- call
) where

-- import Control.Monad
import Control.Monad.Effect.Exception
import Control.Monad.Effect.Internal

data CutFalse = CutFalse
-- data Choose a b = Choose [a] b

-- | Implementation of logical Cut using Exc effects.
cutFalse :: (Exc CutFalse :< r) => Eff r a
cutFalse = throwError CutFalse

{-
call :: (Exc CutFalse :< r) => Eff (Exc CutFalse ': r) a -> Eff r a
call m = loop [] m where
 loop jq (Val x) = return x `mplus` next jq          -- (C2)
 loop jq (E u q) = case decompose u of
    Right (Exc CutFalse) -> mzero  -- drop jq (F2)
    Left u -> check jq u

 check jq u | Just (Choose [] _) <- prj u  = next jq  -- (C1)
 check jq u | Just (Choose [x] k) <- prj u = loop jq (k x)  -- (C3), optim
 check jq u | Just (Choose lst k) <- prj u = next $ map k lst ++ jq -- (C3)
 check jq u = send (\k -> fmap k u) >>= loop jq      -- (C4)

 next []    = mzero
 next (h:t) = loop t h
-}
