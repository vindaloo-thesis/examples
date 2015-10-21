module Transactions

import Effects
import Effect.State
import Effect.StdIO
import Data.HVect
import GeneralStore
import Control.IOExcept
import Ethereum

namespace TestContract
  stash : Nat -> Nat -> IOContract ()
  stash v b = do
    init v b
    printLn $ "v: " ++ show !value ++ ", b: " ++ show !balance
    save 400
    printLn $ "v: " ++ show !value ++ ", b: " ++ show !balance
    finishAndSave

  implicitSave : Contract ()
  implicitSave = do
    init 100 200
    finishAndSave

namespace Main
  main : IO ()
  main = do
    run (stash 100 200)
    run implicitSave

