module Bank

import Effects
import Ethereum
import Ethereum.Types
import Ethereum.GeneralStore

balances : MapField
balances = EMIntInt "balances"

namespace Bank
  deposit : {v : Nat} -> TransEff.Eff ()
            [STORE, ETH (Init v)]
            [STORE, ETH (Running v 0 v)]
  deposit {v} = do
    b <- read balances !sender
    write balances !sender (b+(toIntNat v))
    save v

  withdraw : (a : Nat) -> Eff Bool
             [STORE, ETH (Init 0)]
             (\success => if success
                             then [STORE, ETH (Running 0 a 0)]
                             else [STORE, ETH (Running 0 0 0)])
  withdraw a = do
    b <- read balances !sender
    if b >= (toIntNat a)
       then do
         write balances !sender (b-(toIntNat a))
         send a !sender
         pureM True
       else (pureM False)

namespace Main
  runDep : Nat -> SIO ()
  runDep v = runInit [(),MkS v 0 0] deposit

  runWith : (v : Nat) -> Nat -> {auto p: LTE 0 v} -> SIO Bool
  runWith v a = runInit [(),MkS v 0 0] (withdraw a)

  main : IO ()
  main = return ()

  testList : FFI_Export FFI_Se "testHdr.se" []
  testList = Data Nat "Nat" $
             Data (Bool) "Bool" $
             Fun runDep "deposit" $
             Fun Bank.Main.runWith "withdraw" $
             End

