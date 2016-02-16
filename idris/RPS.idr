module Transactions

import Effects
import Ethereum

playerCount : Field Int
playerCount = MkField 0

players : Field Int Address
players = MkField 1

moves : Field Int Int
moves = MkField 2

winner : Int -> Int -> Int
winner 2 1 = 0 -- Scissors beats paper.
winner 1 2 = 1 -- Scissors beats paper.
winner 0 2 = 0 -- Rock beats scissors.
winner 2 0 = 1 -- Rock beats scissors.
winner 1 0 = 0 -- Paper beats rock.
winner 0 1 = 1 -- Paper beats rock.
winner _ _ = 2 -- Draw

init : Eff () [STORE]
init = write playerCount 0

playerChoice : {auto p : LTE 10 v} -> 
               Int ->
               Eff Bool
               [STORE, ETH v b 0 0, ENV c s o]
               (\succ => if succ
                            then [STORE, ETH v b (v-10) 10, ENV c s o]
                            else [STORE, ETH v b v 0      , ENV c s o])
playerChoice {v} {s} move = do
  pc <- read playerCount
  if pc < 2
   then do
      write players pc s
      write moves pc move
      update playerCount (+ 1)
      keep 10
      send (v-10) s
      pureM True
    else do --If the game is full, return ether
      send v s
      pureM False

--0 : not enough players joined, or invalid value
--1 : player 1
--2 : player 2
--3 : draw
check : {auto p: LTE 20 b} -> Eff Int
        [STORE, ETH 0 b 0 0]
        (\winner => if winner == 0
                       then [STORE, ETH 0 b 0 0]
                       else [STORE, ETH 0 b 20 0])
check = if !(read playerCount) == 2
           then do
             write playerCount 0
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
          else pureM 0
