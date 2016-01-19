module Transactions

import Effects
import Effect.State
import Effect.StdIO
import Ethereum
import Data.Vect
import Decidable.Order
import Data.HVect
import Ethereum.Types
import Ethereum.GeneralStore

playerCount : Field
playerCount = EInt "playerCount"

players : MapField
players = EMIntInt "players"

moves : MapField
moves = EMIntInt "moves"

winner : Int -> Int -> Int
winner 2 1 = 0
winner 1 2 = 1

winner 0 2 = 0
winner 2 0 = 1

winner 1 0 = 0
winner 0 1 = 1

winner _ _ = 2

namespace TestContract
  init : Eff () [STORE]
  init = write playerCount 0

  playerChoice : Int -> {v : Nat} -> { auto p : LTE 10 v } ->
                 Eff Bool
                 [STORE, ETH_IN v]
                 (\succ => if succ
                              then [STORE, ETH_OUT v (v-10) 10]
                              else [STORE, ETH_OUT v v 0])
  playerChoice {v} c = do
    pc <- read playerCount
    if pc < 2
     then do
        save 10
        write players pc !sender
        write moves pc c
        write playerCount (pc+1)
        send (v-10) !sender
        pureM True
      else do
        s <- sender
        send v s
        pureM False

  --0 : not enough players joined, or invalid value
  --1 : player 1
  --2 : player 2
  --3 : draw
  check : Eff Int
          [STORE, ETH_IN 0]
          (\winner => if winner == 0
                         then [STORE, ETH_OUT 0 0 0]
                         else [STORE, ETH_OUT 0 20 0])
  check = if !(read playerCount) == 2
             then do
               let w = winner !(read moves 0) !(read moves 1)
               case w of 
                  0 => do --player 1 wins
                    send 20 !(read players 0)
                    pureM 1
                  1 => do --player 2 wins
                    send 20 !(read players 1)
                    pureM 2
                  otherwise => do --draw
                    send 10 !(read players 0)
                    send 10 !(read players 1)
                    pureM 3
            else do
              pureM 0


namespace Main
  {-
  main : IO ()
  main = do
    res <- runInit [MkS 10 0 0, ()] (playerChoice 0)
    putStrLn . show $ res
    -}

  runInitC : SIO ()
  runInitC = runInit [()] init

  runPC : Int -> SIO Bool
  runPC c = case (lte 10 prim__value) of
                 (Yes p) => runInit [(),MkS prim__value 0 0] (playerChoice c {p})
                 (No _)  => return False

  runCheck : SIO Int
  runCheck = case prim__value == 0 of
                  True  => runInit [(), MkS 0 0 0] check
                  False => return 0

  main : IO ()
  main = return ()

  testList : FFI_Export FFI_Se "testHdr.se" []
  testList = Data Nat "Nat" $
             Data Bool "Bool" $
             --Fun runInitC "init" $
             --Fun runPC "playerChoice" $
             Fun runCheck "check" $
             End

