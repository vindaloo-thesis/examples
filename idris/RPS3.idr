module RPS3

import BoundedList
import Data.Fin
import Effects
import Effect.State

data Address = Addr Int
data Commit a = Comm a
data Choice = Rock | Paper | Scissors

record Game where
  constructor MkGame
  players : BoundedList 2 (Address,Commit Choice)

playerChoice : Commit Choice -> Effects.SimpleEff.Eff () [STATE Game]
playerChoice c with (length !players < FS (FS FZ))
  | True = update (\(MkGame ps) => MkGame ((!sender,c)::ps))

-- finalize : Commit
