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

--((players, choices, playerCount, reward)) = funcs store
derp : minus (plus b (S n)) n = S b
derp = ?derp_rhs

namespace TestContract
  playerFail : Int -> TransEff.Eff Bool [ETHEREUM (Running (S n) b 0 0)]
                          [ETHEREUM (Running (S n) b (S n) 0)]
  playerFail c {n} = do
    send (S n)
    return False

  playerSucc : Int -> TransEff.Eff Bool [ETHEREUM (Running (S n) b 0 0)]
                          [ETHEREUM (Running (S n) b n 1)]
  playerSucc c {n} = do
    send n
    save 1
    return True

  playerChoice : Int -> DepEff.Eff Bool [ETHEREUM (Running (S n) b 0 0)]
                          (\succ => if succ

                          then [ETHEREUM (Running (S n) b n 1)]
                          else [ETHEREUM (Running (S n) b (S n) 0)])
  playerChoice c {n} = do
    let succ = c < 1
    if succ then do
        send n
        save 1
        return succ
    else do
      send (S n)
      return succ




    --pc <- read playerCount
            {-
    if pc < 2
       then ( do save 1; send n; return True)
         --write reward $ (!get reward) + 1
         --write players pc !sender
         --write choices pc c
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

