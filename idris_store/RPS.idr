module Transactions

import Effects
import Effect.State
import Effect.StdIO
import Data.HVect
import GeneralStore
import Control.IOExcept
import Ethereum

store : Store 4
store = [EArray 2 EAddress, EArray 2 EInt, EInt, EInt]


namespace TestContract
  playerChoice : Int -> Eff Bool [ETHEREUM (Running (1+n) b 0 0)]
                          (\succ => if succ
                          then [ETHEREUM (Finished n 1)]
                          else [ETHEREUM (Finished (1+n) 0)])
  playerChoice c {n} = if c < 1 
    then do
      send n
      save 1
      finish
      pureM True
    else do
      send (S n)
      finish
      pureM False
    {-

    do
    let succ = c < 1
    if succ then do
        send n
        save 1
        return succ
    else do
      send (S n)
      return succ
      -}




    --pc <- read playerCount
            {-
    if pc < 2
       then ( do save 1; send n; return True)
         --write reward $ (!read reward ) + 1
         --write (players pc) !sender
         --write (choices pc) c
         --write playerCount (pc + 1)
         --   save 1
        --    return True
       else (do save (S n); send (S n); return False)
                -}

namespace Main
  main : IO ()
  main = do
    printLn "Running stuff"
    --runInit [MkS 100 200] (stash {p=Refl})
    --run implicitSave

