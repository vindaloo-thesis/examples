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

(+) : {a : Nat} -> {b : Nat} -> Exactly a -> Exactly b -> Exactly (a+b)
(+) {a} {b} _ _ = TheNumber (a+b)

(-) : {a: Nat} -> {b: Nat} -> Exactly a -> Exactly b -> {auto smaller : LTE b a} -> Exactly (a-b)
(-) {a} {b} _ _ = TheNumber (a-b)

ETHEREUM : {v : Nat} -> {b : Nat} -> Type -> Exactly b -> EFFECT
ETHEREUM {v} {b} _ _ = MkEff (Exactly v,Exactly b) Ethereum

value : {v: Exactly i} -> Effects.SimpleEff.Eff (Exactly i) [ETHEREUM (Exactly i) b]
value {v} = return v

balance : {b : Exactly i} -> Effects.SimpleEff.Eff (Exactly i) [ETHEREUM (Exactly i) b]
balance {b} = return b

send :  {v : Exactly j} -> (a : Exactly i) -> Address -> {auto smaller : LTE i j} -> Effects.TransEff.Eff () [ETHEREUM (Exactly j) b]
                                            [ETHEREUM (Exactly (j-i)) b]
send _ _ = believe_me ()

{-
save : (a : TheNumber i) -> Eff () [ETHEREUM v b]
save amount = believe_me ()

load : (a : TheNumber i) -> Eff () [ETHEREUM v b]
load _ = believe_me ()
-}

exactlyToNat : {n : Nat} -> Exactly n -> Nat
exactlyToNat {n} (TheNumber n) = n

fromExactly : Exactly n -> Fin (S n)
fromExactly (TheNumber Z)     = FZ
fromExactly (TheNumber (S n)) = FS (fromExactly (TheNumber n))

