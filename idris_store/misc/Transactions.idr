module Transactions

import Effects
import GeneralStore

namespace Ethereum
  data CState = NotRunning | Running Nat Nat Nat | Finished Nat

  data Ethereum : CState -> Type where
    MkS : (value: Nat) -> (balance: Nat) -> (net: Nat) -> Ethereum (Running value balance net)
    MkI : Ethereum NotRunning
    MkF : (net: Nat) -> Ethereum (Finished net)

  data EthereumRules : Effect where
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

  instance Handler EthereumRules m where
    handle (MkS v b (v+b)) Value k    = k v (MkS v b (v+b))
    handle (MkS v b (v+b)) Balance k  = k b (MkS v b (v+b))
    handle (MkS v b (v+b)) (Save a) k = k () (MkS (minus v a) (b+a) (plus (minus v a) (plus b a)))
    handle (MkS v b (v+b)) Finish k   = k () (MkF (v+b))

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

namespace TestContract
  stash : TransEff.Eff () [ETHEREUM (Running v b (v+b))] 
                          [ETHEREUM (Finished (v+b))]
  stash {v} = do 
    save v
    finish

{-
namespace Main
  main : IO ()
  main = do
    runInit [MkS 100 200] (stash )
    -}

