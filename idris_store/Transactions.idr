module Transactions

import Effects
import Effect.State
import Effect.StdIO
import Data.HVect
import GeneralStore
import Control.IOExcept
import Ethereum

es : Nat -> Nat -> EFFECT
es v b = ETHEREUM (TheNumber v) (TheNumber b)


getBalance : {v: Nat} -> {b: Nat} -> SimpleEff.Eff Nat [ETHEREUM (TheNumber v) (TheNumber b)]
getBalance = call $ GetBalance

namespace Contract
  Counter : {v: Nat} -> {b: Nat} -> Type -> Type
  Counter {v} {b} rTy = Eff rTy ['eState ::: es v b]

  getBalance : Counter Nat
  getBalance = do
    b <- ('eState :- Transactions.getBalance)
    return b

namespace Main
  main : IO ()
  main = runInit ['eState := (TheNumber 50, TheNumber 3), ()] (do
    b <- Contract.getBalance
    printLn b)



