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

  runContract : (v : Nat) -> (b : Nat) -> Eff ()
                [ETHEREUM NotRunning]
                [ETHEREUM (Contract 0 100)]
  runContract v b = do
    initContract 0 100
    getBalance
    return ()
namespace User
  User : Type 
  User = DepEff.Eff Nat
                           [ETHEREUM NotRunning, STDIO]
                           (\x => [ETHEREUM (Contract 10 100), STDIO])
  myProg : User 
  myProg = do
    printLn "herp"
    initContract 10 100
    printLn "derp"
    balance
    --runPure $ runContract 5 9

namespace Main
  main : IO ()
  main = printLn $ show !( run myProg)
    --printLn ("Original balance: " ++ (show !(getBalance)))
    --printLn ("Original value: " ++ (show !(getValue)))
    --printLn ("Balance after saving: " ++ (show !(saveAll)))

