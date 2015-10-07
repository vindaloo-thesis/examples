contract RPS
( playerChoice
, finalize ) where

data Choice = Rock | Paper | Scissors
  derives Eq

data Result = First | Second | Tie

winner :: Choice -> Choice -> Result
winner c1 c2  | c1 == c2 = Tie
winner Paper    Rock     = First
winner Rock     Scissors = First
winner Scissors Paper    = First
winner _        _        = Second

{- This is very much an experiment.

The type should be explained: 'Ethereum p s a' is taken to mean
"A value of type 'a' inside the Ethereum monad, with the type
'p' as the state for each participant and 's' as the type of the
contract-wide state."

In this case, 'Commit Choice' is the per-participant state type,
while there is no contract-wide state, and no value is returned.

The semantics of the triple is:
(min participants, max participants, modification)
i.e.
() = playerChoice can be called regardless of how few participants there are.
2  = playerChoice cannot be called when there are already 2 or more participants.
1  = playerChoice increases the number of participants by 1, if it doesn't fail.

'cp_put' is short for current participant_put. It works just as put for the
State monad, but on the current participant's state.
-}
playerChoice ((),2,1) :: Commit Choice -> Ethereum (Commit Choice) () ()
playerChoice = cp_put

{-
'participants' returns a list of all participant addresses.
'p_get' takes an address as input and gives back its state.
('cp_get' would do the same, but only for the current participant.)
-}
finalize (2,(),-2) :: Ethereum (Commit Choice) () ()
finalize = do
    [p1,p2] <- participants
    c1 <- open =<< p_get p1
    c2 <- open =<< p_get p2
    case winner c1 c2 of
      First  -> send (address p1) balance
      Second -> send (address p2) balance
      Tie    -> send (address p1) (balance/2) >> send (address p2) (balance/2)

