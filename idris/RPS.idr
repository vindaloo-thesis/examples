module Transactions

import Effects
import Effect.State
import Effect.StdIO
import Ethereum
import Data.Vect
import Data.HVect
import Types
import GeneralStore

playerCount : Field
playerCount = EInt 0

reward : Field
reward = EInt (index playerCount + size playerCount)

players : Nat -> Field
players i = EAddress (index reward + size reward + i)

moves : Nat -> Field
moves i = EInt (index players + size (players 2) + i)

namespace TestContract
  playerChoice : Int -> {v : Nat} -> { auto p : LTE 10 v } ->
                 Eff Bool [ETH_IN v, STORE] (\succ => if succ then [ETH_OUT (v-10) 10, STORE] else [ETH_OUT v 0, STORE])
  playerChoice {v} c = do
    pc <- read playerCount
    if pc < 2
     then do
        save 10
        write reward (!(read reward)+10)
        write (players (toNat pc)) !sender
        write (moves (toNat pc)) c
        write playerCount (pc+1)
        send (v-10) !sender
        finish
        pureM True
      else do
        s <- sender
        send v s
        finish
        pureM False

  --saveMoney : Int -> Eff Bool [ETH_IN v] (resultEffect [ETH_OUT 0 v] [ETH_OUT v 0])
  {-
  saveMoney : Int -> resultEffect [ETH_IN v] [ETH_OUT 0 v] [ETH_OUT v 0]
  saveMoney {v} input =
    if input == 1
      then do
        save v
        finish
        pureM True
      else do
        send v !sender
        finish 
        pureM False
        -}

--runContract : Eff t 
namespace Main
  main : IO ()
  main = do
    res <- runInit [MkS 10 0 0, ()] (playerChoice 0)
    putStrLn . show $ res

