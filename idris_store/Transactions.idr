module Transactions

import Effects
import Effect.State
import Effect.StdIO
import Data.HVect
import GeneralStore
import Control.IOExcept
import Ethereum

namespace TestContract
  stash : Contract ()
  stash = do 
    save 10
    finish
    {-
    printLn $ "v: " ++ show !value ++ ", b: " ++ show !balance
    save 400
    printLn $ "v: " ++ show !value ++ ", b: " ++ show !balance
    -}

{-
  implicitSave : Contract ()
  implicitSave = do
    init 100 200
    finishAndSave
    -}

namespace Main
  main : IO ()
  main = do
    runInit [MkS 100 200] (stash {p=Refl})
    --run implicitSave

