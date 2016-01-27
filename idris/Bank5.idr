module Bank

import Effects
import Decidable.Order
import Ethereum
import Ethereum.SIO

%default total

balances : MapField
balances = EMIntInt "balances"

namespace Bank
  deposit : {v : Nat} -> TransEff.Eff ()
            [STORE, ETH v b 0 0, ENV c s o]
            [STORE, ETH v b 0 v, ENV c s o]
  deposit {v} {s} = do
    b <- read balances s
    write balances s (b+(toIntNat v))
    save v

  withdraw : (a : Nat) -> Eff Bool
             [STORE, ETH 0 b 0 0, ENV c s o]
             (\success => if success
                             then [STORE, ETH 0 b a 0, ENV c s o]
                             else [STORE, ETH 0 b 0 0, ENV c s o])
  withdraw a {s} = do
    b <- read balances s
    if b >= (toIntNat a)
       then do
         write balances s (b-(toIntNat a))
         send a s
         pureM True
       else (pureM False)

namespace Main
  runDep : SIO ()
  runDep = runInit [(), MkS prim__value prim__selfbalance 0 0, MkE 0x1 0x2 0x2] deposit

  runWith : Nat -> SIO Bool
  runWith a = case prim__value == 0 of
                   True => do
                     runInit [(), MkS 0 prim__selfbalance 0 0, MkE 0 0x00cf7667b8dd4ece1728ef7809bc844a1356aadf 0] (withdraw a)
                     return True
                   False => return False

  main : IO ()
  main = return ()

  testList : FFI_Export FFI_Se "testHdr.se" []
  testList = Data Nat "Nat" $
             Data (Bool) "Bool" $
             Fun runDep "deposit" $
             Fun Bank.Main.runWith "withdraw" $
             End

