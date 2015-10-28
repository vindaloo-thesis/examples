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
  playerChoice : Int -> {n : Nat} -> (v : Nat) -> Contract Bool
                          (\succ => if succ
                          then [ETHEREUM (Finished n 1)]
                          else [ETHEREUM (Finished v 0)])
  playerChoice c v {n} = do
    if c < 1 -- !(read pc) < 1 
      then do
        send n "sender"
        save 1
        finish
        pureM True
      else do
        send v "sender"
        finish
        pureM False

  runContract : Contract () ce
  runContract = do
    xx <- runInit [Running 1 100 0 0]  (playerChoice 1 100)
    pureM ()
namespace Main
  main : IO ()
  --main = runInit [["XYZ"]] (playerChoice 1 100)
    --runInit [()] (playerChoice 1 100)
    
    --runInit [(), Ethereum (Running 100 100 0 0)] (playerChoice 1 100)
    --runInit [()] (playerChoice 1 100)
  main = printLn "ss"
    --return ()
    --run implicitSave

