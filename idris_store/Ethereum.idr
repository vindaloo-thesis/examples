module Effects.Ethereum

import Effects
import Data.Fin
import Data.So
import GeneralStore

------------ TYPES -----------------

data Commit a = Comm a
data Address = Addr Int

------------- PROOF ----------------
{-
lte' : Nat -> Nat -> Type  
lte' Z k         = ()      -- 0 ≤ k  
lte' (S k) Z     = Void    -- ¬(k + 1 ≤ 0)  
lte' (S k) (S l) = lte k l -- k ≤ l → k + 1 ≤ l + 1  


LTEProof' : {m, n : Nat} -> {auto prf : lte' m n} -> LTE m n  
LTEProof' {m=Z}   {n=k}         = LTEZero  
LTEProof' {m=S k} {n=Z}   {prf} = absurd prf  
LTEProof' {m=S k} {n=S l} {prf} = LTESucc $ LTEProof' {m=k} {n=l} {prf}  
-}
IsLte : Ord e => (x:e) -> (y:e) -> Type
IsLte x y = So (x <= y)


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
---- Ethereum return_type resource_in (r : return_type -> resource_out)
data CState = NotRunning | Contract Nat Nat

instance Default CState where
  default = NotRunning

data Ethereum : CState -> Type where
  MkS : (value: Nat) -> (balance: Nat) -> Ethereum (Contract value balance)
  MkI : Ethereum NotRunning

instance Default (Ethereum (Contract v b)) where
  default {v} {b} = MkS v b

instance Default (Ethereum NotRunning) where
  default = MkI

initContract : (v : Nat) -> (b : Nat) -> Ethereum (Contract v b)
initContract v b = MkS v b

data EthereumRules : Effect where
  Init    : (v : Nat) -> (b : Nat) ->
            sig EthereumRules ()
            (Ethereum NotRunning)
            (Ethereum (Contract v b))
  Value   : sig EthereumRules Nat (Ethereum h)
  Balance : sig EthereumRules Nat (Ethereum h)
  Save    : (a : Nat) -> {auto p: LTE a v} ->
            sig EthereumRules ()
            (Ethereum (Contract v b))
            (Ethereum (Contract (v-a) (b+a)))
  Finish  : sig EthereumRules ()
            (Ethereum (Contract 0 b))
            (Ethereum NotRunning)

ETHEREUM : CState -> EFFECT
ETHEREUM h = MkEff (Ethereum h) EthereumRules

-- k result_value updated_resource
instance Handler EthereumRules m where
  handle MkI (Init v b) k      = k () (MkS v b)
  handle (MkS v b) Value k    = k v (MkS v b)
  handle (MkS v b) Balance k  = k b (MkS v b)
  handle (MkS v b) (Save a) k = k () (MkS (v-a) (b+a))
  handle (MkS Z b) Finish k   = k () MkI

value : Eff Nat [ETHEREUM (Contract v b)]
value = call $ Value

balance : Eff Nat [ETHEREUM (Contract v b)]
balance = call $ Balance

save : (a : Nat) -> {v: Nat} -> {auto p: LTE a v} -> Eff () [ETHEREUM (Contract v b)] [ETHEREUM (Contract (v-a) (b+a))]
save a = call $ Save a

finish : Eff () [ETHEREUM (Contract 0 b)] [ETHEREUM NotRunning]
finish = call Finish
