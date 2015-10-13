module RPS

import Effect.State

record Game (nPlayers : Nat) where
  constructor MkGame
  players : Vec nPlayers (Address,Commit Choice)

playerChoice : Commit Choice -> { [STATE (Game n)] ==> [STATE (Game (S n))] } Eff ()
playerChoice c = update ((!sender,c)::)

payWinner : Game 2 -> { [ETHEREUM] } Eff ()
payWinner (MkGame [(p1,c1),(p2,c2)]) = case winner !(open c1) !(open c2) of
  First  => send !balance p1
  Second => send !balance p2
  Tie    => let payout = !balance/2
             in send payout p1 >> send payout p2