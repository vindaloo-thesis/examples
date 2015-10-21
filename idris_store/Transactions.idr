module Transactions

import Effects
import Effect.State
import Effect.StdIO
import Data.HVect
import GeneralStore
import Control.IOExcept
import Ethereum

namespace TestContract

  implSave : {v: Nat} -> {b: Nat} -> IOContract v b () 
  implSave = do
    printLn !value
    printLn !balance
    saveAndFinish

  explSave : {v: Nat} -> {b: Nat} -> Contract v b () 
  explSave {v} = do
    v' <- value
    save v --plz idris, LTE v v is trivial, no?
    finish


namespace Main
  main : IO ()
  main = do
    runIOContract 10 100 implSave
    runContract 100 100 explSave

