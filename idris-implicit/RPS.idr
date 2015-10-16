module RPS

import Effects
import Ethereum

data Choice = Rock | Paper | Scissors

instance Eq Choice where
  Scissors == Scissors = True
  Rock     == Rock     = True
  Paper    == Paper    = True
  c1       == c2       = False


data Result = First | Second | Tie

winner : Choice -> Choice -> Result
winner c1 c2 = if c1 == c2
                then Tie
                else case (c1,c2) of           
                        (Scissors, Paper   ) => First
                        (Paper   , Rock    ) => First
                        (Rock    , Scissors) => First
                        (c1,c2)              => Second

