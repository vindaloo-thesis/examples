module Effects.Ethereum

import Effects
import Data.Fin
import GeneralStore

data Address = Addr Int

data Exactly : Nat -> Type where
  TheNumber : (n : Nat) -> Exactly n
instance Show (Exactly n) where
  show _ = "x"

data Ethereum : Effect where
  getBalance : sig Ethereum Nat (Exactly v, Exactly b) (\x => Nat)
  {-
  Get : Ethereum a  a (\x => a)
  Put : Ethereum () a (\x => b)
  -}

ETHEREUM : {v : Nat} -> {b : Nat} -> Type -> Exactly b -> EFFECT
ETHEREUM {v} {b} _ _ = MkEff (Exactly v,Exactly b) Ethereum



data Commit a = Comm a


instance Default (Exactly n) where
  default = TheNumber n

(+) : {a : Nat} -> {b : Nat} -> Exactly a -> Exactly b -> Exactly (a+b)
(+) {a} {b} _ _ = TheNumber (a+b)

(-) : {a: Nat} -> {b: Nat} -> Exactly a -> Exactly b -> {auto smaller : LTE b a} -> Exactly (a-b)
(-) {a} {b} _ _ = TheNumber (a-b)

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

