{-# language TypeApplications #-}
{-# language ScopedTypeVariables #-}
module Main where

import Data.Union
import Control.Monad
import Control.Monad.Effect
import Control.Monad.Effect.Exception 

data Foo = Foo deriving (Eq,Show)

oops :: forall effs. (Member (Exc Foo) effs, 
                      Member (Lift IO) effs,
                      PureEffect (Data.Union.Union effs) 
                      ) 
     => Eff effs ()
oops = Control.Monad.Effect.Exception.bracket @effs @Eff @()
                       (pure ())
                       (\() -> putStrLn "finalizer called")
                       (\() -> throwError Foo)

main :: IO ()
main = do
    r1 <- runM (runError @_ @_ @Foo oops)
    print r1 -- finalizer is not called

