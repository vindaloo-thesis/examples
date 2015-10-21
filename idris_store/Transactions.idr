module Transactions

import Effects
import Effect.State
import Effect.StdIO
import Data.HVect
import GeneralStore
import Control.IOExcept
import Ethereum
import Data.So

namespace TestContract


  implSave : IOContract () v b
  implSave = do
    printLn $ "v: " ++ show !value ++ ", b: " ++ show !balance
    saveAndFinish

  explSave : IOContract () v b
  explSave {v} = do
    printLn $ "v: " ++ show !value ++ ", b: " ++ show !balance
    save {p=Refl} v
    printLn $ "v: " ++ show !value ++ ", b: " ++ show !balance
    finish


namespace Main
  main : IO ()
  main = do
    runIOContract 10 100 implSave
    runIOContract 1 100 explSave

