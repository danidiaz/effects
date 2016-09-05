module Tests.Fresh where

import Control.Monad
import Control.Monad.Effect
import Control.Monad.Effect.Fresh

makeFresh :: Int -> Eff r Int
makeFresh n = runFresh' (liftM last (replicateM n fresh)) 0

testFresh :: Int -> Int
testFresh = run . makeFresh
