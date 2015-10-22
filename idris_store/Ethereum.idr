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
  MkS : (value: Nat) -> (balance: Nat) -> (out: Nat) -> Ethereum (Running value balance out)
  MkI : Ethereum NotRunning
  MkF : (net: Nat) -> Ethereum (Finished net)

instance Default (Ethereum NotRunning) where
  default = MkI

data EthereumRules : Effect where
  Init    : (v : Nat) -> (b : Nat) ->
            sig EthereumRules ()
            (Ethereum NotRunning)
            (Ethereum (Running v b 0))
  Value   : sig EthereumRules Nat (Ethereum (Running v b t))
  Balance : sig EthereumRules Nat (Ethereum (Running v b t))
  Save    : (a : Nat) -> 
            sig EthereumRules ()
            (Ethereum (Running v b t))
            (Ethereum (Running (minus v a) (b+a) t))
  Send    : (a : Nat) ->
            sig EthereumRules ()
            (Ethereum (Running v b t))
            (Ethereum (Running v (minus b a) (t+a)))
  Finish  : sig EthereumRules ()
            (Ethereum (Running v b t))
            (Ethereum (Finished t))

ETHEREUM : CState -> EFFECT
ETHEREUM h = MkEff (Ethereum h) EthereumRules

-- k result_value updated_resource
instance Handler EthereumRules m where
  handle MkI (Init v b) k      = k () (MkS v b 0)
  handle (MkS v b t) Value k    = k v (MkS v b t)
  handle (MkS v b t) Balance k  = k b (MkS v b t)
  handle (MkS v b t) (Save a) k = k () (MkS (minus v a) (b+a) t)
  handle (MkS v b t) (Send a) k = k () (MkS v (minus b a) (t+a))
  handle (MkS v b t) Finish k   = k () (MkF t)

IOContract : Type -> Type
IOContract r = {v: Nat} -> {b: Nat} -> {t : Nat } -> TransEff.Eff r [ETHEREUM (Running v b t), STDIO]
                               [ETHEREUM (Finished t), STDIO]

Contract : Type -> Type
Contract r = {v: Nat} -> {b: Nat} -> {t : Nat} -> TransEff.Eff r [ETHEREUM (Running v b t)]
                               [ETHEREUM (Finished t)]

{-
init : (v : Nat) -> (b : Nat) -> Eff ()
       [ETHEREUM NotRunning]
       [ETHEREUM (Running v b)]
init v b = call $ Init v b
-}

value : Eff Nat [ETHEREUM (Running v b t)]
value = call $ Value

balance : Eff Nat [ETHEREUM (Running v b t)]
balance = call $ Balance

save : (a : Nat) -> Eff ()
       [ETHEREUM (Running v b t)]
       [ETHEREUM (Running (minus v a) (plus b a) t)]
save a = call $ Save a

send : (a : Nat) -> Eff ()
       [ETHEREUM (Running v b t)]
       [ETHEREUM (Running v (minus b a) (plus t a))]
send a = call $ Send a

finish : Eff ()
         [ETHEREUM (Running v b t)]
         [ETHEREUM (Finished t)]
finish = call Finish
