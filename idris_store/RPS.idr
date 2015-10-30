module Transactions

import Effects
import Effect.State
--import Effect.StdIO
import Effect.StdIO
import Ethereum

--store : Store 4
--store = [EArray 2 EAddress, EArray 2 EInt, EInt, EInt]

-- runContract : Applicative m => (v : Nat) -> (c : Contract x ce) -> m x
-- runContract v c = runInit [Running 100 100 0 0] c
namespace TestContract
  playerChoice : Int -> (v : Nat) -> {auto p : LTE 10 v} -> Contract Bool
                          (\succ => if succ
                          then [ETHEREUM (Finished (v-10) 10)]
                          else [ETHEREUM (Finished v 0)])
  playerChoice c v = do
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
{-
  runContract : Contract () ce
  runContract = do
    xx <- runInit [MkS 1 100 0 0]  (playerChoice 1 100)
    pureM ()
-}

namespace Main
  main : IO ()
  main = do
    res <- runInit [MkS 1 100 0 0] (playerChoice 0 100)
    putStrLn . show $ res
    --runInit [()] (playerChoice 1 100)
    
    --runInit [(), Ethereum (Running 100 100 0 0)] (playerChoice 1 100)
    --runInit [()] (playerChoice 1 100)
  -- main = printLn "ss"
    --return ()
    --run implicitSave

