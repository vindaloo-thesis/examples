module Transactions

import Effects
import Effect.State
import Effect.StdIO
import Data.HVect
import GeneralStore
import Control.IOExcept
import Ethereum

namespace MyContract
  MyContract : {v : Nat} -> {b : Nat} -> Type -> Type
  MyContract {v} {b} rTy = TransEff.Eff rTy
                           [ETHEREUM (Contract v b)]
                           [ETHEREUM (Contract v b)]
  getBalance : MyContract Nat
  getBalance = return !(balance)

  initContract : (v : Nat) -> (b : Nat) -> Eff ()
                  [ETHEREUM NotRunning]
                  [ETHEREUM (Contract v b)]
  initContract v b = call $ Init v b

namespace User
  User : Type 
  User = Eff () [ETHEREUM NotRunning, STDIO]
  myProg : User 
  myProg = do
    initContract 0 100
    printLn (show !(balance))
    save 0
    printLn (show !(balance))
    finish

namespace Main
  main : IO ()
  main = run myProg

