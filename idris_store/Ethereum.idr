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
data Ethereum : Effect where
  Balance : sig Ethereum Nat (Exactly v, Exactly b)
  Value   : sig Ethereum Nat (Exactly v, Exactly b)
  --Save    : (a: Nat) -> {auto p: LTE a v} -> sig Ethereum Nat (Exactly v, Exactly b) (\a' => (Exactly (v-a'), Exactly (b+a')))
  Save    : (a: Nat) -> {auto p: LTE a v} -> Ethereum (Exactly v, Exactly b) (Exactly v, Exactly b) (\(TheNumber v', TheNumber b') => (Exactly v', Exactly b'))

--sig Ethereum Nat (Exactly v, Exactly b) (\a' => (Exactly (v-a'), Exactly (b+a')))

ETHEREUM : {v : Nat} -> {b : Nat} -> Exactly v -> Exactly b -> EFFECT
ETHEREUM {v} {b} _ _ = MkEff (Exactly v,Exactly b) Ethereum

-- k result_value updated_resource
instance Handler Ethereum m where
  -- covering handle : (r : res) -> (eff : e t res resk) ->
                    --(k : ((x : t) -> resk x -> m a)) -> m a
  --handle (MkPair v b) Balance  = handle' {v=v} {b=b} (MkPair v b) Balance 
  --handle (MkPair v b) Value    k = k (exactlyToNat v) (MkPair v b)
  --handle (MkPair (TheNumber v') b) (Save a) k = let a' = TheNumber a in k () (MkPair (TheNumber (v'-a)) (b+a'))

	handle (MkPair v b) (Save a) k = let a' = TheNumber a in k (default, default) (default, default)

balance : {v: Nat} -> {b: Nat} -> SimpleEff.Eff Nat [ETHEREUM (TheNumber v) (TheNumber b)]
balance = call $ Balance

value : {v: Nat} -> {b: Nat} -> SimpleEff.Eff Nat [ETHEREUM (TheNumber v) (TheNumber b)]
value = call $ Value

{-
save : {v: Nat} -> {b: Nat} -> (a: Nat) -> {p: LTE a v} ->
       TransEff.Eff () [ETHEREUM (TheNumber v) (TheNumber b)]
                       [ETHEREUM (TheNumber (v-a)) (TheNumber (b+a))]
-}
save : {v: Nat} -> {b: Nat} -> (a: Nat) -> {p: LTE a v} ->
       DepEff.Eff (v',b') [ETHEREUM (TheNumber v) (TheNumber b)]
                       (\(a ** LTE a v) => [ETHEREUM (TheNumber (v-a)) (TheNumber (b+a))])
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

