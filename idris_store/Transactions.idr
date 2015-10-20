module Transactions

import Effects
import Effect.State
import Effect.StdIO
import Data.HVect
import GeneralStore
import Control.IOExcept
import Ethereum

namespace TestContract
  stash : IOContract
  stash = do
    init 11 100
    printLn (show !(balance))
    save 9
    save 1
    save 1
    printLn (show !(balance))
    finish

  implicitSave : Contract
  implicitSave = do
    init 100 200
    finishAndSave

namespace Main
  main : IO ()
  main = do
    run stash
    run implicitSave

