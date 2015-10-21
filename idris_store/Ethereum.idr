module Effects.Ethereum

import Effects
import Data.Fin
import Data.So
import Effect.StdIO

import GeneralStore

------------ TYPES -----------------

data Commit a = Comm a
data Address = Addr Nat


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
  Save    : (a : Nat) ->
            UpdateEffect.sig EthereumRules ()
            (Ethereum (Running v b))
            (Ethereum (Running (minus v a) (b+a)))
  Finish  : sig EthereumRules ()
            (Ethereum (Running Z b))
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
  handle (MkR v b) (Save a) k = k () (MkR (minus v a) (b+a))
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

--save : (a : Nat) -> {v : Nat} -> {auto p: LTE a v} -> Eff ()
save : (a : Nat) -> {v : Nat} -> {h: EFFECT} -> TransEff.Eff ()
--save : (a : Nat) -> Eff ()
-- save : (a : Nat) -> {auto p: LTE a v} -> Eff ()
--save : (a : Nat) -> {default proof {trivial; } p: LTE a v} -> Eff ()
       [ETHEREUM (Running v b)]
       --[ETHEREUM (if a <= v then (Running v' b') else Finished) ]
       --[ETHEREUM (if a <= v then (Running (minus v a) (b+a)) else Finished) ]
       [h]
       {-
       (\succ => if succ
          then [ETHEREUM (Running (minus v a) (b+a))]
          else [ETHEREUM Finished]
          )
          -}
(Eff () [ETHEREUM (Running v b)] [ETHEREUM (Running (minus v a) (b+a))]) <== save = call $ Save a
{-
save a  = call (Save a)
save a = call FinishSave
-}

saveAndFinish : Eff ()
         [ETHEREUM (Running v b)]
         [ETHEREUM Finished]
saveAndFinish = call FinishSave

finish : Eff ()
         [ETHEREUM (Running 0 b)]
         [ETHEREUM Finished]
finish = call Finish


Contract : Type -> Nat -> Nat -> Type
Contract r v b = TransEff.Eff r [ETHEREUM (Running v b)] [ETHEREUM Finished]

runContract : (v: Nat) -> (b: Nat) -> Contract r v b -> IO r
runContract v b c = runInit [MkS] (do init v b; c)


IOContract : Type -> Nat -> Nat -> Type
IOContract r v b = TransEff.Eff r [ETHEREUM (Running v b), STDIO] [ETHEREUM Finished, STDIO]

runIOContract : (v: Nat) -> (b: Nat) -> IOContract r v b -> IO r
runIOContract v b c = runInit [MkS, ()] (do init v b; c)

%hint
lemmaMinus : (v : Nat) -> minus v v = 0
lemmaMinus Z     = Refl
lemmaMinus (S n) = lemmaMinus n


{-
IOEtherContract : Nat -> Nat -> Type -> Type
IOEtherContract v b rTy = Eff rTy [ETHEREUM (Running v b), STDIO] [ETHEREUM Finished, STDIO]

IOContract : Type -> Type
IOContract r = {v: Nat} -> {b: Nat} -> IOEtherContract v b r

runIOContract : (v : Nat) -> (b : Nat) -> IOEtherContract v b r -> IO r
runIOContract v b c = runInit [MkS, ()] 
  (do (init v b)
      c
  )
  -}
