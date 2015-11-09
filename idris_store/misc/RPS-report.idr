module Transactions

import Effects
import Ethereum
import Example

-- Store is a type in the Ethereum library that describes the schema of the store.
-- We have constructed a function that generates an instance of the store of the proper type,
-- with associated functions for retrieving elements from the store.
-- TODO: Generate writing functions.
-- TODO: Make part of Ethereum Effect (or its own Effect). 
-- TODO: Syntax rewrite to not have to verbosely assign functions separately.
{-
store : Store 4
store = [ (players, EArray 2 EAddress)
        , (choices, EArray 2 EInt)
        , (playerCount, EInt)
        , (reward, EInt)]
        -}


namespace RPS
  -- Contract is a type in the Ethereum library that corresponds to an Effect.
  -- The Effect resource contains (this is subject to change):
  ----v: Value of ether sent with the call
  ----b: Current ether balance stored in contract
  ----t: ether spent on outgoing transactions during the call (initially 0)
  ----s: ether explicitly marked as "saved" during the call (initially 0)
  -- In this example, n is the value of ether exceeding the cost of playing the game.
  -- The implicit proof states this, and it's something that the compiler will automatically ensure.
  -- In this example, the input parameter c is an integer corresponding to the choice of rock, paper or scissors
  playerChoice : Int -> {n : Nat} -> (v : Nat) -> {auto p: v = (n+1000)} -> Contract Bool
                          (\succ => if succ 
                          -- If return value is true, then t=n and s=1 (success)
                          then [ETHEREUM (Finished n 1000)]
                          -- Otherwise, t=v and s=0 (failure, so return everything)
                          else [ETHEREUM (Finished v 0)])
  playerChoice c v {n} = do
    pc <- read playerCount
    if pc < 1 
      then do
        save 1000
        write reward (!(read reward)+1000)
        write (players pc) !sender
        write (choices pc) c
        send n "sender"
        finish
        pureM True 
        --pureM instead of return allows us to use this value to decide the output resource type as done in the signature.
      else do
        send v "sender"
        finish
        pureM False

-- Exported functions (in this case, playerChoice) get fed to a runContract function in the standard library that ensures that the input resource is instantiated properly.
-- It should also either: 
-- * Ensure that (t+s)=v. That is, all ether supplied should be explicitly saved.
-- * Alternatively, return (v-(t+s)) to the sender. 
-- Which of these we choose to do is still not decided. Either way, the Ethereum Effect needs to keep track of s and t and compare it to v,
