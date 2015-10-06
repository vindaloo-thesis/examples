contract RPS
( playerChoice
, finalize
) where

data Choice = Rock | Paper | Scissors
  derives Eq

data Result = First | Second | Tie

data PlayerData = PlayerData { address    :: Address
                             , commitment :: Commit Choice }

type GameState = [PlayerData]

-- This is a library function for the Ethereum monad. Not sure how
-- to implement it.
open :: Commit a -> Ethereum s a
open c = if {- c has been opened -}
          then {- return its value -}
          else {- fail and rollback everything -}

{- Because the state (GameState) contains values of type 'Commit a',
we will also generate a function to reveal these commitments which will
be exposed in the contract's interface but is never part of the source
code. Only after it has been successfully revealed can 'open' succeed. -}

winner :: Choice -> Choice -> Result
winner c1 c2  | c1 == c2 = Tie
winner Paper    Rock     = First
winner Rock     Scissors = First
winner Scissors Paper    = First
winner _        _        = Second

{- The syntax below is probably not the best way to solve this, but it
basically means that the max number of participants is 2, and that
this function will add 1 to the number of participants iff it succeeds.

If a call is not valid due to the number of participants, all ether is
be returned to the sender. -}
playerChoice (2,1) :: Commit Choice -> Ethereum GameState ()
playerChoice c = modify (PlayerData sender c:)



{- Same here. Max participants is 2, and if finalize succeeds -2 will be
added to the current number of participants (i.e. it will reset to 0).

Thought: maybe we want to show that this function can only be run if we
already have 2 participants? In this case it is enforced by the calls to
'open', but this can not be expected in the general case. -}
finalize (2,-2) :: Ethereum GameState ()
finalize = do
    GameState p1 p2 <- gets
    c1 <- open (commitment p1)
    c2 <- open (commitment p2)
    put []
    case winner c1 c2 of
      First  -> send (address p1) balance
      Second -> send (address p2) balance
      Tie    -> send (address p1) (balance/2) >> send (address p2) (balance/2)

{-
General remark:

We are updating the state quite explicitly above (see lines 42 and 57).
A better idea might be to define a per-participant state, with special syntax
to update it. The compiler would take care of mapping addresses to their states.
This will have to be explored in later experiments though.
-}
