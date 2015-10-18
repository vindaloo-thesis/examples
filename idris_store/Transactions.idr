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

namespace Contract
  Counter : {v: Nat} -> {b: Nat} -> Type -> Type
  --Counter {v} {b} rTy = Eff rTy ['es ::: es v b] [es 0 b']
  Counter {v} {b} rTy = Eff rTy ['es ::: es v b] -- [es 0 10]

  getBalance : Counter Nat
  getBalance = do
    b <- 'es :- balance
    return b

  getValue : Counter Nat
  getValue = return !('es :- value)

  saveAll : Counter Nat
  saveAll = do
    v <- ('es :- value)
    --'es :- save v
    return !('es :- balance)

namespace Main
  main : IO ()
  main = runInit ['es := (TheNumber 5, TheNumber 30), ()] (do
    printLn ("Original balance: " ++ (show !(getBalance)))
    printLn ("Original value: " ++ (show !(getValue)))
    printLn ("Balance after saving: " ++ (show !(saveAll)))
  )



