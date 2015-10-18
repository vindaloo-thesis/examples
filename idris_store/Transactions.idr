module Transactions

import Effects
import Effect.State
import Effect.StdIO
import Data.HVect
import GeneralStore
import Control.IOExcept
import Ethereum

es : EFFECT
es = ETHEREUM (TheNumber 100) (TheNumber 500)

instance Handler Ethereum m where
  handle (MkPair v b) GetBalance k = k (exactlyToNat v) (MkPair v b)

getBalance : {v: Nat} -> {b: Nat} -> SimpleEff.Eff Nat [ETHEREUM (TheNumber v) (TheNumber b)]
getBalance = call $ GetBalance

namespace Contract
  Counter : Type -> Type
  Counter rTy = Eff rTy ['eState ::: es]

  gb : Counter Nat
  gb = do
    b <- ('eState :- getBalance)
    return b

namespace User
  User : Type -> Type
  User rTy = Eff rTy ['eState ::: es
                     , STDIO]

myProg : User ()
myProg = printLn (runPure ('eState :- gb))

namespace Main
  main : IO ()
  main = run myProg



