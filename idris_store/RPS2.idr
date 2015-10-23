import Data.BoundedList
import Ethereum
import Effects

data GState = Game Nat (List Address) (List Nat)

data RPS : GState -> Type where
  MkI : RPS (Game 0 [] [])
  MkG : (np : Nat) -> (ps : List Address) -> (cs : List Nat) -> RPS (Game np ps cs)

data RPSRules : Effect where
  PlayerChoice : (c : Nat) -> (p : Address) ->
    DepUpdateEffect.sig RPSRules Bool
    (RPS (Game np ps cs))
    (\succ => if succ
              then (RPS (Game (S np) (p::ps) (c::cs)))
              else (RPS (Game np ps cs )))

  

