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
  GetBalance : sig Ethereum Nat (Exactly v, Exactly b)

instance Handler Ethereum m where
  handle (MkPair v b) GetBalance k = k (exactlyToNat v) (MkPair v b)

ETHEREUM : {v : Nat} -> {b : Nat} -> Exactly v -> Exactly b -> EFFECT
ETHEREUM {v} {b} _ _ = MkEff (Exactly v,Exactly b) Ethereum






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

