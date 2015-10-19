module Effects.Ethereum

import Effects
import Data.Fin
import GeneralStore

------------ TYPES -----------------

data Commit a = Comm a
data Address = Addr Int

-------------- EXACTLY --------------
data Exactly : Nat -> Type where
  TheNumber : (n : Nat) -> Exactly n

exactlyToNat : {n : Nat} -> Exactly n -> Nat
exactlyToNat {n} (TheNumber n) = n

fromExactly : Exactly n -> Fin (S n)
fromExactly (TheNumber Z)     = FZ
fromExactly (TheNumber (S n)) = FS (fromExactly (TheNumber n))

(+) : {a : Nat} -> {b : Nat} -> Exactly a -> Exactly b -> Exactly (a+b)
(+) {a} {b} _ _ = TheNumber (a+b)

(-) : {a: Nat} -> {b: Nat} -> Exactly a -> Exactly b -> {auto smaller : LTE b a} -> Exactly (a-b)
(-) {a} {b} _ _ = TheNumber (a-b)

instance Default (Exactly n) where
  default = TheNumber n

-------------- EFFECT --------------
data Ethereum : Effect where
  Balance : sig Ethereum Nat (Exactly v, Exactly b)
  Value   : sig Ethereum Nat (Exactly v, Exactly b)
  Save    : (a: Nat) -> {auto p: LTE a v} -> sig Ethereum () (Exactly v, Exactly b) (Exactly (v-a), (Exactly (b+a)))

ETHEREUM : {v : Nat} -> {b : Nat} -> Exactly v -> Exactly b -> EFFECT
ETHEREUM {v} {b} _ _ = MkEff (Exactly v,Exactly b) Ethereum

instance Handler Ethereum m where
  handle (MkPair v b) Balance  k = k (exactlyToNat b) (MkPair v b)
  handle (MkPair v b) Value    k = k (exactlyToNat v) (MkPair v b)
  handle (MkPair v b) (Save a) k = let a' = TheNumber a in k () (MkPair (v-a') (b+a'))

balance : {v: Nat} -> {b: Nat} -> SimpleEff.Eff Nat [ETHEREUM (TheNumber v) (TheNumber b)]
balance = call $ Balance

value : {v: Nat} -> {b: Nat} -> SimpleEff.Eff Nat [ETHEREUM (TheNumber v) (TheNumber b)]
value = call $ Value

--save : {v: Nat} -> {b: Nat} -> (a: Nat) -> {default proof {trivial; } p: LTE a v} ->
save : {v: Nat} -> {b: Nat} -> (a: Nat) -> {p: LTE a v} ->
       TransEff.Eff () [ETHEREUM (TheNumber v) (TheNumber b)]
                       [ETHEREUM (TheNumber (v-a)) (TheNumber (b+a))]
save a = call $ Save a





{-
value : {v: Exactly i} -> Effects.SimpleEff.Eff (Exactly i) [ETHEREUM (Exactly i) b]
value {v} = return v

balance : {b : Exactly i} -> Effects.SimpleEff.Eff (Exactly i) [ETHEREUM (Exactly i) b]
balance {b} = return b

send :  {v : Exactly j} -> (a : Exactly i) -> Address -> {auto smaller : LTE i j} -> Effects.TransEff.Eff () [ETHEREUM (Exactly j) b]
                                            [ETHEREUM (Exactly (j-i)) b]
send _ _ = believe_me ()
-}

{-
save : (a : TheNumber i) -> Eff () [ETHEREUM v b]
save amount = believe_me ()

load : (a : TheNumber i) -> Eff () [ETHEREUM v b]
load _ = believe_me ()
-}

