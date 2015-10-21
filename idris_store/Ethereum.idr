module Effects.Ethereum

import Effects
import Data.Fin
import Data.So
import Effect.StdIO

import GeneralStore

------------ TYPES -----------------

data Commit a = Comm a
data Address = Addr Int


-------------- EFFECT --------------
data CState = NotRunning | Running Nat Nat Nat | Finished Nat

data Ethereum : CState -> Type where
  MkS : (value: Nat) -> (balance: Nat) -> (net: Nat) -> {auto p: value+balance=net} -> Ethereum (Running value balance net)
  MkI : Ethereum NotRunning
  MkF : (net: Nat) -> Ethereum (Finished net)

instance Default (Ethereum NotRunning) where
  default = MkI

data EthereumRules : Effect where
  Init    : (v : Nat) -> (b : Nat) ->
            sig EthereumRules ()
            (Ethereum NotRunning)
            (Ethereum (Running v b (v+b)))
  Value   : sig EthereumRules Nat (Ethereum (Running v b (v+b)))
  Balance : sig EthereumRules Nat (Ethereum (Running v b (v+b)))
  Save    : (a : Nat) -> 
            sig EthereumRules ()
            (Ethereum (Running v b (v+b)))
            (Ethereum (Running (minus v a) (b+a) (plus (minus v a) (plus b a))))
  Finish  : sig EthereumRules ()
            (Ethereum (Running v b (v+b)))
            (Ethereum (Finished (v+b)))

ETHEREUM : CState -> EFFECT
ETHEREUM h = MkEff (Ethereum h) EthereumRules

-- k result_value updated_resource
instance Handler EthereumRules m where
  handle MkI (Init v b) k      = k () (MkS v b (v+b))
  handle (MkS v b (v+b)) Value k    = k v (MkS v b (v+b))
  handle (MkS v b (v+b)) Balance k  = k b (MkS v b (v+b))
  handle (MkS v b (v+b)) (Save a) k = k () (MkS (minus v a) (b+a) (plus (minus v a) (plus b a)))
  handle (MkS v b (v+b)) Finish k   = k () (MkF (v+b))

IOContract : Type -> Type
IOContract r = {v: Nat} -> {b: Nat} -> TransEff.Eff r [ETHEREUM (Running v b (v+b)), STDIO]
                               [ETHEREUM (Finished (v+b)), STDIO]

Contract : Type -> Type
Contract r = {v: Nat} -> {b: Nat} -> TransEff.Eff r [ETHEREUM (Running v b (v+b))]
                               [ETHEREUM (Finished (v+b))]

{-
init : (v : Nat) -> (b : Nat) -> Eff ()
       [ETHEREUM NotRunning]
       [ETHEREUM (Running v b)]
init v b = call $ Init v b
-}

value : Eff Nat [ETHEREUM (Running v b (v+b))]
value = call $ Value

balance : Eff Nat [ETHEREUM (Running v b (v+b))]
balance = call $ Balance

save : (a : Nat) -> Eff ()
       [ETHEREUM (Running v b (v+b))]
       [ETHEREUM (Running (minus v a) (plus b a) ((minus v a) + (b+a)))]
save a = call $ Save a

finish : Eff ()
         [ETHEREUM (Running v b (v+b))]
         [ETHEREUM (Finished (v+b))]
finish = call Finish
