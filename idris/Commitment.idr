module RPS2

import Effects
import Ethereum

data Address = Addr Int
data Commit a = Comm a
data Choice = Rock | Paper | Scissors

record Game where
  constructor MkGame
  player1 : Maybe (Address,Commit Choice)
  player2 : Maybe (Address,Commit Choice)

finalize : Eff () [ETHEREUM Game]
  case !(state) of
      (MkGame (Just (p1,c1)) (Just (p2,c2))) =>
          case winner !(open c1) !(open c2) of
            First  => send !balance p1
            Second => send !balance p2
            Tie    => let payout = !balance/2
                       in send payout p1 >> send payout p2
      otherwise => return ()
