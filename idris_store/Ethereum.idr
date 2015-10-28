module Effects.Ethereum

import Effects
import Data.Fin
import Data.So
import Effect.StdIO
import Effect.Exception
import Control.IOExcept
import EVM

--import GeneralStore


--perf older version, clean and rebuild
------------ TYPES -----------------
data Commit a = Comm a
data Address = Addr String


-------------- EFFECT --------------
data CState = NotRunning | Running Nat Nat Nat Nat | Finished Nat Nat
Init : Nat -> Nat -> CState
Init v b = Running v b 0 0

data Ethereum : CState -> Type where
  MkS : (value: Nat) -> (balance: Nat) -> (trans: Nat) -> (saved: Nat) -> Ethereum (Running value balance trans saved)
  MkI : Ethereum NotRunning
  MkF : (trans: Nat) -> (saved: Nat) -> Ethereum (Finished trans saved)

{-
instance Show (Ethereum m) where
  show = "derp"

instance Show CState where
  show = "herp"
  -}
instance Default CState where
  default = Init 1 100

instance Default (Ethereum NotRunning) where
  default = MkI

instance Default (Ethereum (Running v b 0 0)) where
  default {v} {b} = MkS v b 0 0 

data EthereumRules : Effect where
  Value   : sig EthereumRules Nat
            (Ethereum (Running v b t s))
  Sender   : sig EthereumRules String
            (Ethereum (Running v b t s))
  Save    : (a : Nat) -> 
            sig EthereumRules ()
            (Ethereum (Running v b t s))
            (Ethereum (Running v b t (s+a)))
  Send    : (a : Nat) ->
            sig EthereumRules ()
            (Ethereum (Running v b t s))
            (Ethereum (Running v b (t+a) s))
  Finish  : sig EthereumRules ()
            (Ethereum (Running v b t s))
            (Ethereum (Finished t s))
  Read : {x: Type} -> (a: x) -> sig EthereumRules x
            (Ethereum (Running v b t s))
  Write : sig EthereumRules ()
            (Ethereum (Running v b t s))


ETHEREUM : CState -> EFFECT
ETHEREUM h = MkEff (Ethereum h) EthereumRules

Contract : (x : Type) -> (ce : x -> List EFFECT) -> Type
Contract x ce = {m : Type -> Type} -> {b : Nat} -> {v : Nat} -> EffM m x [ETHEREUM (Init v b)] ce
-- k result_value updated_resource
{-
instance Handler EthereumRules IO where
  handle (MkS v b t s) Value k = k v (MkS v b t s)
  handle (MkS v b t s) (Save a) k = do printLn ("Saved " ++ show v); k () (MkS v b t (s+a))
  handle (MkS v b t s) (Send a) k = k () (MkS v b (t+a) s)
  handle (MkS v b t s) (Finish) k = k () (MkF t s)

instance Handler EthereumRules (IOExcept a) where
  handle r Value k = k 0 r
  handle (MkS v b t s) (Save a) k = do ioe_lift (printLn ("Saved " ++ show v)); k () (MkS v b t (s+a))
  handle (MkS v b t s) (Send a) k = k () (MkS v b (t+a) s)
  handle (MkS v b t s) Finish k = k () (MkF t s)
  -}

instance Handler EthereumRules m where
  handle (MkS v b t s) Value k = k v (MkS v b t s)
  handle (MkS v b t s) (Save a) k = k () (MkS v b t (s+a))
  handle (MkS v b t s) (Send a) k = k () (MkS v b (t+a) s)
  handle (MkS v b t s) (Finish) k = k () (MkF t s)
  handle (MkS v b t s) Sender k = k "senderxyz" (MkS v b t s)
  handle (MkS v b t s) (Read a) k = k a (MkS v b t s)
  handle (MkS v b t s) Write k = k () (MkS v b t s)

sender : Eff String
       [ETHEREUM (Running v b t s)]
sender = call $ Sender

read : {x : Type} -> (a: x) -> Eff x
       [ETHEREUM (Running v b t s)]
read a = call $ Read a

write : (a: x) -> (c : y) -> Eff ()
       [ETHEREUM (Running v b t s)]
write _ _ = call $ Write

value : Eff Nat
       [ETHEREUM (Running v b t s)]
value = call $ Value

save : (a : Nat) -> Eff ()
       [ETHEREUM (Running v b t s)]
       [ETHEREUM (Running v b t (s+a))]
save a = call $ Save a

send : (a : Nat) -> (r : String) -> Eff ()
       [ETHEREUM (Running v b t s)]
       [ETHEREUM (Running v b (plus t a) s)]
send a r = call $ Send a

--TODO: Wrap pureM here too. Doesn't seem to work right now.
--finish ret = call (Finish ret) >>= (\_ => pureM ret)

finish : Eff ()
         [ETHEREUM (Running v b t s)]
         [ETHEREUM (Finished t s)]
finish = call Finish

