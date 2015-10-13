module RPS3

import Data.BoundedList

data Address = Addr Int
data Commit a = Comm a
data Choice = Rock | Paper | Scissors

record Game where
  constructor MkGame
  players : BoundedList 2 (Address,Commit Choice)

using (g : Game)
  data Finalizable : Game -> Type where
    TwoPlayers : length g = 2 -> Finalizable g