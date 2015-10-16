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

ETHEREUM : {v : Nat} -> {b : Nat} -> Type -> Exactly b -> EFFECT
ETHEREUM {v} {b} _ _ = MkEff (Exactly v,Exactly b) Ethereum

(+) : {a : Nat} -> {b : Nat} -> Exactly a -> Exactly b -> Exactly (a+b)
(+) {a} {b} _ _ = TheNumber (a+b)

(-) : (a: Type) -> (b : Type) -> Type
(-) a b with (a,b)
    | (Exactly i,Exactly j) = Exactly (i-j)
    | otherwise             = ()

value : {v : Exactly i} -> Effects.SimpleEff.Eff (Exactly i) [ETHEREUM v b]
value {v} = return v

balance : {b : Exactly i} -> Effects.SimpleEff.Eff (Exactly i) [ETHEREUM v b]
balance {b} = return b

send : (a : Exactly i) -> Address -> Eff () [ETHEREUM v b]
                                            [ETHEREUM (v-a) b]
send _ _ = believe_me ()

save : (a : Exactly i) -> Eff () [ETHEREUM v b]
                                 [ETHEREUM (v-a) (b + a)]
save amount = believe_me ()

saveAll : Effects.TransEff.Eff () [ETHEREUM v b]
                 [ETHEREUM (TheNumber 0) (b+v)]
saveAll = believe_me ()

load : (a : Exactly i) -> Eff () [ETHEREUM v b]
                                 [ETHEREUM (v+a) (b-a)]
load _ = believe_me ()

loadAll : Eff () [ETHEREUM v b]
                 [ETHEREUM v (TheNumber 0)]
loadAll = believe_me ()

open : Commit a -> Eff a [ETHEREUM v b]
open (Comm a) = return a

sender : Eff Address [ETHEREUM v b]
sender = return $ Addr 0

exactlyToNat : {n : Nat} -> Exactly n -> Nat
exactlyToNat {n} (TheNumber n) = n

fromExactly : Exactly n -> Fin (S n)
fromExactly (TheNumber Z)     = FZ
fromExactly (TheNumber (S n)) = FS (fromExactly (TheNumber n))