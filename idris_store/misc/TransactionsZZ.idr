module Transactions

import Data.ZZ
import Effects
import GeneralStore

namespace Ethereum
  data CState = NotRunning | Running ZZ ZZ ZZ | Finished ZZ

  data Ethereum : CState -> Type where
    MkS : (value: ZZ) -> (balance: ZZ) -> (net: ZZ) -> Ethereum (Running value balance net)
    MkI : Ethereum NotRunning
    MkF : (net: ZZ) -> Ethereum (Finished net)

  data EthereumRules : Effect where
    Value   : sig EthereumRules ZZ (Ethereum (Running v b (v+b)))
    Balance : sig EthereumRules ZZ (Ethereum (Running v b (v+b)))
    Save    : (a : ZZ) -> 
              UpdateEffect.sig EthereumRules ()
              (Ethereum (Running v b (v+b)))
              (Ethereum (Running (v-a) (b+a) ((v-a)+(b+a))))
    Finish  : sig EthereumRules ()
              (Ethereum (Running v b (v+b)))
              (Ethereum (Finished (v+b)))

  ETHEREUM : CState -> EFFECT
  ETHEREUM h = MkEff (Ethereum h) EthereumRules

  instance Handler EthereumRules m where
    handle (MkS v b (v+b)) Value k    = k v (MkS v b (v+b))
    handle (MkS v b (v+b)) Balance k  = k b (MkS v b (v+b))
    handle (MkS v b (v+b)) (Save a) k = k () (MkS (v-a) (b+a) ((v-a)+(b+a)))
    handle (MkS v b (v+b)) Finish k   = k () (MkF (v+b))

  value : Eff ZZ [ETHEREUM (Running v b (v+b))]
  value = call $ Value

  balance : Eff ZZ [ETHEREUM (Running v b (v+b))]
  balance = call $ Balance

  save : (a : ZZ) -> Eff ()
         [ETHEREUM (Running v b (v+b))]
         [ETHEREUM (Running (v-a) (b+a) ((v-a) + (b+a)))]
  save a = call $ Save a

  finish : Eff ()
           [ETHEREUM (Running v b (v+b))]
           [ETHEREUM (Finished (v+b))]
  finish = call Finish

namespace TestContract
  stash : TransEff.Eff () [ETHEREUM (Running v b (v+b))] 
                          --[ETHEREUM (Finished (v+b))]
                          [ETHEREUM (Running  (subZ v 10) (b+10) (v+b))]
  stash = do 
    save 10
    --finish

{-
namespace Main
  main : IO ()
  main = do
    runInit [MkS 100 200] (stash )
    -}


