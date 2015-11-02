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
reward = EInt 1

store : Schema 2
store = [playerCount, reward]

namespace TestContract
  playerChoice : Int -> { auto p : LTE 10 v } ->
                 Eff Bool [ETH_IN v, STORE] (resultEffect [ETH_OUT (v-10) 10, STORE] [ETH_OUT v 0, STORE])
  playerChoice {v} c = do
    pc <- read playerCount
    if pc < 2
      then do
        save 10
        write reward (!(read reward)+10)
        write playerCount (pc+1)
        send (v-10) "sender"
        finish
        pureM True
      else do
        send v "sender"
        finish
        pureM False

  saveMoney : Int -> Eff Bool [ETH_IN v] (resultEffect [ETH_OUT 0 v] [ETH_OUT v 0])
  saveMoney {v} input =
    if input == 1
      then do
        save v
        finish
        pureM True
      else do
        send v "sender"
        finish
        pureM False

namespace Main
  main : IO ()
  main = do
    res <- runInit [MkS 10 0 0, store] (playerChoice 0)
    putStrLn . show $ res

