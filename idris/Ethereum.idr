module Effects.Ethereum

import Effects
import Data.Fin

data Address = Addr Word32

data Ethereum : Effect where
--  Get :      sig State a  a
--  Put : b -> sig State () a b

ETHEREUM : Nat -> EFFECT
ETHEREUM v b = MkEff v b Ethereum

value : Eff (v : Nat) [ETHEREUM v b]

balance : Eff (b : Nat) [Ethereum v b]

send : Address -> (a : Fin v) -> Eff () [ETHEREUM v b]
                                        [ETHEREUM (v - (finToNat a)) b]

save : (a : Fin v) -> Eff () [ETHEREUM v b]
                             [ETHEREUM (v - (finToNat a)) (b + (finToNat a))]

load : (a : Fin b) -> Eff () [Ethereum v b]
                             [ETHEREUM (v + (finToNat a)) (b - (finToNat a))]