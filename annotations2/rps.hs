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

(.=) is the "set" operator from the Lens library.
Here it is assumed that it refers to the per-participant state,
but we will have to make a distinction between the two states'
operators if we are to actually separate the state into two.
-} 
playerChoice (2,1) :: Commit Choice -> Ethereum (Commit Choice) () ()
playerChoice = put

