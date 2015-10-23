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
data CState = NotRunning | Running Nat Nat Nat Nat | Finished Nat Nat

data Ethereum : CState -> Type where
  MkS : (value: Nat) -> (balance: Nat) -> (trans: Nat) -> (saved: Nat) -> Ethereum (Running value balance trans saved)
  MkI : Ethereum NotRunning
  MkF : (trans: Nat) -> (saved: Nat) -> Ethereum (Finished trans saved)

instance Default (Ethereum NotRunning) where
  default = MkI

data EthereumRules : Effect where
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

ETHEREUM : CState -> EFFECT
ETHEREUM h = MkEff (Ethereum h) EthereumRules

-- k result_value updated_resource
instance Handler EthereumRules m where
  handle (MkS v b t s) (Save a) k = k () (MkS v b t (s+a))
  handle (MkS v b t s) (Send a) k = k () (MkS v b (t+a) s)
  handle (MkS v b t s) (Finish) k = k () (MkF t s)

save : (a : Nat) -> Eff ()
       [ETHEREUM (Running v b t s)]
       [ETHEREUM (Running v b t (s+a))]
save a = call $ Save a

send : (a : Nat) -> Eff ()
       [ETHEREUM (Running v b t s)]
       [ETHEREUM (Running v b (plus t a) s)]
send a = call $ Send a

--TODO: Wrap pureM here too. Doesn't seem to work right now.
--finish ret = call (Finish ret) >>= (\_ => pureM ret)

finish : Eff ()
         [ETHEREUM (Running v b t s)]
         [ETHEREUM (Finished t s)]
finish = call Finish

