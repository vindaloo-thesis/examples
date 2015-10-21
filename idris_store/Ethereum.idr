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
data CState = NotRunning | Running Nat Nat

data Ethereum : CState -> Type where
  MkS : (value: Nat) -> (balance: Nat) -> Ethereum (Running value balance)
  MkI : Ethereum NotRunning

instance Default (Ethereum NotRunning) where
  default = MkI

data EthereumRules : Effect where
  Init    : (v : Nat) -> (b : Nat) ->
            sig EthereumRules ()
            (Ethereum NotRunning)
            (Ethereum (Running v b))
  Value   : sig EthereumRules Nat (Ethereum (Running v b))
  Balance : sig EthereumRules Nat (Ethereum (Running v b))
  Save    : (a : Nat) -> {auto p: LTE a v} ->
            sig EthereumRules ()
            (Ethereum (Running v b))
            (Ethereum (Running (v-a) (b+a)))
  Finish  : sig EthereumRules ()
            (Ethereum (Running 0 b))
            (Ethereum NotRunning)
  FinishSave : sig EthereumRules ()
            (Ethereum (Running v b))
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
  handle (MkS v b) FinishSave k = k () MkI

IOContract : Type
IOContract = Eff () [ETHEREUM NotRunning, STDIO]

Contract : Type
Contract = Eff () [ETHEREUM NotRunning]

init : (v : Nat) -> (b : Nat) -> Eff ()
       [ETHEREUM NotRunning]
       [ETHEREUM (Running v b)]
init v b = call $ Init v b

value : Eff Nat [ETHEREUM (Running v b)]
value = call $ Value

balance : Eff Nat [ETHEREUM (Running v b)]
balance = call $ Balance

save : (a : Nat) -> {auto p: LTE a v} -> Eff ()
       [ETHEREUM (Running v b)]
       [ETHEREUM (Running (v-a) (b+a))]
save a = call $ Save a

finishAndSave : Eff ()
         [ETHEREUM (Running v b)]
         [ETHEREUM NotRunning]
finishAndSave = call FinishSave

finish : Eff ()
         [ETHEREUM (Running 0 b)]
         [ETHEREUM NotRunning]
finish = call Finish
