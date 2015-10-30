module Transactions

import Effects
import Effect.State
import Effect.StdIO
import Ethereum

namespace TestContract
  playerChoice : Int -> { auto p : LTE 10 v } ->
                 DepEff.Eff Bool [ETH_IN v] (resultEffect [ETH_OUT (v-10) 10] [ETH_OUT v 0])
  playerChoice {v} c =
    if c < 1 -- !(read pc) < 1 
      then do
        send (v-10) "sender"
        save 10
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


--  runContract : Nat -> Nat -> Contract () ce -> 
--  runContract c = do
--    xx <- runInit [MkS 1 100 0 0] (playerChoice 0 100)
--    pureM ()


namespace Main
  main : IO ()
  main = do
    res <- runInit [MkS 10 0 0] (playerChoice 0)
    putStrLn . show $ res

