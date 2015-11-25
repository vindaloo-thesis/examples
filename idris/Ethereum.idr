module Effects.Ethereum

import Effects
import Data.Fin
import Data.So
import Effect.StdIO
import Effect.Exception
import Control.IOExcept
import EVM
import GeneralStore



--perf older version, clean and rebuild
------------ TYPES -----------------
data Commit a = Comm a
data Address = Addr String


-------------- EFFECT --------------
data CState = NotRunning | Running Nat Nat Nat | Finished Nat Nat
Init : Nat -> CState
Init v = Running v 0 0

data Ethereum : CState -> Type where
  MkS : (value: Nat) -> (trans: Nat) -> (saved: Nat) -> Ethereum (Running value trans saved)
  MkI : Ethereum NotRunning
  MkF : (trans: Nat) -> (saved: Nat) -> Ethereum (Finished trans saved)

instance Default CState where
  default = Init 1

instance Default (Ethereum NotRunning) where
  default = MkI

instance Default (Ethereum (Running v 0 0)) where
  default {v} = MkS v 0 0 

--TODO: Can we remove Finish here and just use Running?
data EthereumRules : Effect where
  Value   : sig EthereumRules Nat
            (Ethereum (Running v t s))
  Balance : sig EthereumRules Nat
            (Ethereum (Running v t s))
  Sender   : sig EthereumRules String
            (Ethereum (Running v t s))
  Save    : (a : Nat) -> 
            sig EthereumRules ()
            (Ethereum (Running v t s))
            (Ethereum (Running v t (s+a)))
  Send    : (a : Nat) ->
            (r : String) ->
            sig EthereumRules ()
            (Ethereum (Running v t s))
            (Ethereum (Running v (t+a) s))
  Finish  : sig EthereumRules ()
            (Ethereum (Running v t s))
            (Ethereum (Finished t s))


ETHEREUM : CState -> EFFECT
ETHEREUM h = MkEff (Ethereum h) EthereumRules

Contract : (x : Type) -> (ce : x -> List EFFECT) -> Type
Contract x ce = {m : Type -> Type} -> {v : Nat} -> EffM m x [ETHEREUM (Init v)] ce
-- k result_value updated_resource

instance Handler EthereumRules IO where
  handle (MkS v t s) Value    k = k v (MkS v t s)

  handle (MkS v t s) Balance  k = k 100 (MkS v t s) -- TODO: Change this. Balance should be *read*.

  handle (MkS v t s) (Save a) k = do putStrLn $ "- Saved " ++ show a
                                     k () (MkS v t (s+a))

  handle (MkS v t s) (Send a r) k = do putStrLn $ "- Sent  " ++ show a ++ " to " ++ show r
                                       k () (MkS v (t+a) s)

  handle (MkS v t s) (Finish) k = do putStrLn "\n"
                                     putStrLn "FINISHED"
                                     putStrLn "--------"
                                     putStrLn $ "Value:   " ++ show v
                                     putStrLn $ "Sent:   -" ++ show t
                                     putStrLn $ "Saved:  -" ++ show s
                                     putStrLn   "-------------"
                                     putStrLn $ "Sum:     " ++ show (minus v (t+s))
                                     k () (MkF t s)

  handle (MkS v t s) Sender k   = k "senderxyz" (MkS v t s)


ETH_IN : Nat -> EFFECT
ETH_IN v = ETHEREUM (Init v)

--TODO: Should return "running", not "finished". runContract should finish
ETH_OUT : Nat -> Nat -> EFFECT
ETH_OUT t s = ETHEREUM (Finished t s)

resultEffect : (i : List EFFECT) -> (t : List EFFECT) -> (f : List EFFECT) -> Type 
resultEffect i t f = Eff Bool i (\cond => if cond then t else f)
--resultEffect t f cond = if cond then t else f
resultEffect' : {v : Nat} -> (tt: Nat) -> (ts : Nat) -> (ft : Nat) -> (fs : Nat) -> Type 
resultEffect' {v} tt ts ft fs = DepEff.Eff Bool [ETH_IN v, STORE] (\cond => if cond then [ETH_OUT tt ts, STORE]  else [ETH_OUT ft fs, STORE])

sender : Eff String
       [ETHEREUM (Running v t s)]
sender = call $ Sender

value : Eff Nat
       [ETHEREUM (Running v t s)]
value = call $ Value

save : (a : Nat) -> Eff ()
       [ETHEREUM (Running v t s)]
       [ETHEREUM (Running v t (s+a))]
save a = call $ Save a

send : (a : Nat) -> (r : String) -> Eff ()
       [ETHEREUM (Running v t s)]
       [ETHEREUM (Running v (plus t a) s)]
send a r = call $ Send a r

--TODO: Wrap pureM here too. Doesn't seem to work right now.
--finish ret = call (Finish ret) >>= (\_ => pureM ret)

--finish : {auto p : LTE v (t+s)} -> Eff ()
finish : Eff ()
         [ETHEREUM (Running v t s)]
         [ETHEREUM (Finished t s)]
finish = call Finish

