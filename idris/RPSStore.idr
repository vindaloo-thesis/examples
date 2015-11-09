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

RPSStore : Schema 2 
RPSStore = [playerCount, reward]

namespace TestContract
  -- For some reason, setting store explicitly doesn't typecheck
  --updatePlayers : SimpleEff.Eff Integer [STORE RPSStore] 
  updatePlayers : SimpleEff.Eff Integer [STORE] 
  updatePlayers  = do
		x <- read playerCount
		write playerCount (x+1)
		return x

namespace Main
  main : IO ()
  main = do
    x <- run (updatePlayers)
    putStrLn (show x)


