module RPS

import Effects
import Effect.State
import Data.Vect
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

record Game (nPlayers : Fin 3) where
  constructor MkGame
  players : Vect (finToNat nPlayers) (Address,Commit Choice)

playerChoice : Commit Choice -> Effects.SimpleEff.Eff () [STATE (Game n)]
                                       --[STATE (Game (FS n))]
playerChoice c = _ -- update ((!sender,c)::)

addPlayer : Game n -> (Address,Commit Choice) -> Game n

payWinner : Game 2 -> Effects.TransEff.Eff () [ETHEREUM v b] [ETHEREUM (TheNumber 0) (TheNumber 0)]
payWinner (MkGame [(p1,c1),(p2,c2)]) = do
  loadAll
  v <- value
  send v p1

 {-
  case winner !(open c1) !(open c2) of
    First  => value >>= \v > send (fromExactly v) p1
    Second => send (fromExactly !value) p2
    Tie    => let payout = !value/2
               in send payout p1 >> send payout p2
-}