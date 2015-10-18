module Transactions

import Effects
import Effect.State
import Effect.StdIO
import Data.HVect
import GeneralStore
import Control.IOExcept
import Ethereum


store : Store 1
store = [EInt]

storet : Type
storet = interp store

istore : HVect [Int]
istore = [0]

cnt : HVect [Int] -> Int
cnt [i] = i

es : EFFECT
es = ETHEREUM {v=100} storet (TheNumber 100)

instance Handler Ethereum m where
  handle (MkPair v b) getBalance k = k 150 150
  --handle (Exactly v, Exactly b) getBalance 

namespace Contract
  Counter : Type -> Type
  Counter rTy = Eff rTy ['eState ::: es]
  gb : Counter Nat
  gb = pure  (!('eState :- getBalance))

namespace User
  User : Type -> Type
  User rTy = Eff rTy ['eState ::: es
                     , STDIO]

myProg : User ()
myProg = printLn !('eState :- gb)

instance Default t => Default (HVect [t]) where
  default = [default]

namespace Main
  main : IO ()
  main = run myProg



