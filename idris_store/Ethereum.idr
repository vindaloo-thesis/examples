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
data CState = Starting | Running Nat Nat | Finished

data Ethereum : CState -> Type where
  MkS : Ethereum Starting
  MkR : (value: Nat) -> (balance: Nat) -> Ethereum (Running value balance)
  MkF : Ethereum Finished

instance Default (Ethereum Starting) where
  default =  MkS

data EthereumRules : Effect where
  Init    : (v : Nat) -> (b : Nat) ->
            sig EthereumRules ()
            (Ethereum (Starting))
            (Ethereum (Running v b))
  Value   : sig EthereumRules Nat (Ethereum (Running v b))
  Balance : sig EthereumRules Nat (Ethereum (Running v b))
  Save    : (a : Nat) -> {auto p: LTE a v} ->
            sig EthereumRules ()
            (Ethereum (Running v b))
            (Ethereum (Running (v-a) (b+a)))
  Finish  : sig EthereumRules ()
            (Ethereum (Running 0 b))
            (Ethereum Finished)
  FinishSave : sig EthereumRules ()
            (Ethereum (Running v b))
            (Ethereum Finished)

ETHEREUM : CState -> EFFECT
ETHEREUM h = MkEff (Ethereum h) EthereumRules

-- k result_value updated_resource
instance Handler EthereumRules m where
  handle MkS (Init v b) k      = k () (MkR v b)
  handle (MkR v b) Value k    = k v (MkR v b)
  handle (MkR v b) Balance k  = k b (MkR v b)
  handle (MkR v b) (Save a) k = k () (MkR (v-a) (b+a))
  handle (MkR Z b) Finish k   = k () MkF
  handle (MkR v b) FinishSave k = k () MkF

init : (v : Nat) -> (b : Nat) -> Eff ()
       [ETHEREUM Starting]
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

saveAndFinish : Eff ()
         [ETHEREUM (Running v b)]
         [ETHEREUM Finished]
saveAndFinish = call FinishSave

finish : Eff ()
         [ETHEREUM (Running 0 b)]
         [ETHEREUM Finished]
finish = call Finish

IOEtherContract : Nat -> Nat -> Type -> Type
IOEtherContract v b rTy = Eff rTy [ETHEREUM (Running v b), STDIO] [ETHEREUM Finished, STDIO]

EtherContract : Nat -> Nat -> Type -> Type
EtherContract v b rTy = Eff rTy [ETHEREUM (Running v b)] [ETHEREUM Finished]

Contract : Type -> Type
Contract r = {v: Nat} -> {b: Nat} -> EtherContract v b r

IOContract : Type -> Type
IOContract r = {v: Nat} -> {b: Nat} -> IOEtherContract v b r

runContract : (v : Nat) -> (b : Nat) -> EtherContract v b r -> IO r
runContract v b c = run 
  (do (init v b)
      c
  )

runIOContract : (v : Nat) -> (b : Nat) -> IOEtherContract v b r -> IO r
runIOContract v b c = runInit [MkS, ()] 
  (do (init v b)
      c
  )
