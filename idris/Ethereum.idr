module Effects.Ethereum

import Effects
import Data.Fin

data Address = Addr Int

data Ethereum : Effect where
--  Get :      sig State a  a
--  Put : b -> sig State () a b

data Commit a = Comm a

data Exactly : Nat -> Type where
  TheNumber : (n : Nat) -> Exactly n

ETHEREUM : Nat -> Nat -> EFFECT
ETHEREUM v b = MkEff (Exactly v,Exactly b) Ethereum

value : {v : Nat} -> Effects.SimpleEff.Eff (Exactly v) [ETHEREUM v b]
value {v} = return (TheNumber v)

balance : {b : Nat} -> Effects.SimpleEff.Eff (Exactly b) [ETHEREUM v b]
balance {b} = return (TheNumber b)

send : (a : Fin (S v)) -> Address -> Eff () [ETHEREUM v b]
                                            [ETHEREUM (v - (finToNat a)) b]
send amount to = believe_me ()

save : (a : Fin (S v)) -> Eff () [ETHEREUM v b]
                                 [ETHEREUM (v - (finToNat a)) (b + (finToNat a))]
save amount = believe_me ()

saveAll : Eff () [ETHEREUM v b]
                 [ETHEREUM 0 (b+v)]
saveAll = believe_me ()

load : (a : Fin (S b)) -> Eff () [ETHEREUM v b]
                                 [ETHEREUM (v + (finToNat a)) (b - (finToNat a))]
load amount = believe_me ()

loadAll : Eff (Exactly (v+b)) [ETHEREUM v b]
                              [ETHEREUM (v+b) 0]
loadAll = believe_me ()

open : Commit a -> Eff a [ETHEREUM v b]
open (Comm a) = return a

sender : Eff Address [ETHEREUM v b]
sender = return $ Addr 0

fromExactly : Exactly n -> Fin (S n)
fromExactly (TheNumber Z)     = FZ
fromExactly (TheNumber (S n)) = FS (fromExactly (TheNumber n))