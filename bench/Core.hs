{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveFunctor #-}
module Main where

import Control.Monad
import Control.Monad.Effect
import Control.Monad.Effect.Internal
import Control.Monad.Effect.Exception
import Control.Monad.Effect.State

import Criterion
import Criterion.Main
import qualified Control.Monad.State as MTL
import qualified Control.Monad.Except as MTL
import qualified Control.Monad.Free as Free

--------------------------------------------------------------------------------
                        -- State Benchmarks --
--------------------------------------------------------------------------------
oneGet :: Int -> (Int, Int)
oneGet n = run (runState get n)

countDown :: Int -> (Int,Int)
countDown start = run (runState go start)
  where go = get >>= (\n -> if n <= 0 then pure n else put (n-1) >> go)

countDownMTL :: Int -> (Int,Int)
countDownMTL = MTL.runState go
  where go = MTL.get >>= (\n -> if n <= 0 then pure n else MTL.put (n-1) >> go)

--------------------------------------------------------------------------------
                       -- Exception + State --
--------------------------------------------------------------------------------
countDownExc :: Int -> Either String (Int,Int)
countDownExc start = run $ runError (runState go start)
  where go = get >>= (\n -> if n <= (0 :: Int) then throwError "wat" else put (n-1) >> go)

countDownExcMTL :: Int -> Either String (Int,Int)
countDownExcMTL = MTL.runStateT go
  where go = MTL.get >>= (\n -> if n <= (0 :: Int) then MTL.throwError "wat" else MTL.put (n-1) >> go)

--------------------------------------------------------------------------------
                          -- Freer: Interpreter --
--------------------------------------------------------------------------------
data Http out where
  Open :: String -> Http ()
  Close :: Http ()
  Post  :: String -> Http String
  Get   :: Http String

open' :: Member Http r => String -> Eff r ()
open'  = send . Open

close' :: Member Http r => Eff r ()
close' = send Close

post' :: Member Http r => String -> Eff r String
post' = send . Post

get' :: Member Http r => Eff r String
get' = send Get

runHttp :: Eff (Http ': e) b -> Eff e b
runHttp (Val b) = pure b
runHttp (E u q) = case decompose u of
  Right (Open _) -> runHttp (q `apply` ())
  Right Close    -> runHttp (q `apply` ())
  Right (Post a) -> runHttp (q `apply` a)
  Right Get      -> runHttp (q `apply` "")
  Left u'        -> E u' $ tsingleton (runHttp . apply q)

--------------------------------------------------------------------------------
                          -- Free: Interpreter --
--------------------------------------------------------------------------------
data FHttpT x
  = FOpen String x
  | FClose x
  | FPost String (String -> x)
  | FGet (String -> x)
    deriving Functor

type FHttp = Free.Free FHttpT

fopen' :: String -> FHttp ()
fopen' s = Free.liftF $ FOpen s ()

fclose' :: FHttp ()
fclose' = Free.liftF $ FClose ()

fpost' :: String -> FHttp String
fpost' s = Free.liftF $ FPost s id

fget' :: FHttp String
fget' = Free.liftF $ FGet id

runFHttp :: FHttp a -> Maybe a
runFHttp (Free.Pure x) = pure x
runFHttp (Free.Free (FOpen _ n)) = runFHttp n
runFHttp (Free.Free (FClose n))  = runFHttp n
runFHttp (Free.Free (FPost s n)) = pure s  >>= runFHttp . n
runFHttp (Free.Free (FGet n))    = pure "" >>= runFHttp . n

--------------------------------------------------------------------------------
                        -- Benchmark Suite --
--------------------------------------------------------------------------------
prog :: Member Http r => Eff r ()
prog = open' "cats" >> get' >> post' "cats" >> close'

prog' :: FHttp ()
prog' = fopen' "cats" >> fget' >> fpost' "cats" >> fclose'

p :: Member Http r => Int -> Eff r ()
p count   =  open' "cats" >> replicateM_ count (get' >> post' "cats") >>  close'

p' :: Int -> FHttp ()
p' count  = fopen' "cats" >> replicateM_ count (fget' >> fpost' "cats") >> fclose'

main :: IO ()
main =
  defaultMain [
    bgroup "State" [
        bench "get"          $ whnf oneGet 0
    ],
    bgroup "Countdown Bench" [
        bench "effects.State"    $ whnf countDown 10000
      , bench "mtl.State"      $ whnf countDownMTL 10000
    ],
    bgroup "Countdown+Except Bench" [
        bench "effects.ExcState"  $ whnf countDownExc 10000
      , bench "mtl.ExceptState" $ whnf countDownExcMTL 10000
    ],
    bgroup "HTTP Simple DSL" [
        bench "effects"  $ whnf (run . runHttp) prog
      , bench "effects"  $ whnf runFHttp prog'

      , bench "effectsN" $ whnf (run . runHttp . p) 100000
      , bench "effectsN" $ whnf (runFHttp . p')     100000
    ]
  ]
