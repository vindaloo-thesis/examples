contract RPS
( playerChoice
, finalize ) where

import Control.Lens ((%=),(+=),(^.),(.=))
import Data.Map (empty,assocs)

state = { players  :: Map Address (Commit Choice)
        , nPlayers :: Int
        }

init :: Ethereum ()
init = do
    players  .= empty
    nPlayers .= 0

playerChoice :: Commit Choice -> Ethereum ()
playerChoice c | state^.nPlayers == 2 = fail "Game full"
               | otherwise            = do players %= insert sender c
                                           nPlayers += 1 -- (+=) is the increment operator for lenses

finalize :: Ethereum ()
finalize | state^.nPlayers /= 2 = fail "No game to finalize"
         | otherwise            = do
    c1 <- open cc1
    c2 <- open cc2
    case winner c1 c2 of
      First  -> send (address p1) balance
      Second -> send (address p2) balance
      Tie    -> send (address p1) (balance/2) >> send (address p2) (balance/2)
  where
    (p1,cc1):(p2,cc2):_ = assocs (state^.players)

winner :: Choice -> Choice -> Result
winner c1 c2  | c1 == c2 = Tie
winner Paper    Rock     = First
winner Rock     Scissors = First
winner Scissors Paper    = First
winner _        _        = Second