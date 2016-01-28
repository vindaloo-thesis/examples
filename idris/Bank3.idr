module Bank

import Effects
import Decidable.Order
import Ethereum
import Ethereum.SIO

%default total

Owners : List Address
Owners = [0x00cf7667b8dd4ece1728ef7809bc844a1356aadf
         ,0x004a7617b84d4ece1728ef7809bc844356a897ba
         ]

deposit : Eff () [ETH v b 0 0] [ETH v b 0 v]
deposit {v} = save v

withdraw : (a : Nat) -> {auto p: LTE a b} -> Eff Bool
           [ETH 0 b 0 0, ENV c s o]
           (\success => if success
                           then [ETH 0 b a 0, ENV c s o]
                           else [ETH 0 b 0 0, ENV c s o])
withdraw a {s} = if s `elem` Owners 
                    then do
                      send a s
                      pureM True
                    else pureM False

namespace Main
  runDep : Nat -> SIO ()
  runDep v = runInit [MkS prim__value 0 0 0] deposit

  runWith : Nat -> SIO Bool
  runWith a = case (lte a prim__selfbalance) of
                   (Yes p) => case prim__value == 0 of
                                   True => do
                                     runInit [MkS 0 prim__selfbalance 0 0, MkE 0 0x00cf7667b8dd4ece1728ef7809bc844a1356aadf 0] (withdraw a {p})
                                     return True
                                   False => return False
                   (No _)  => return False

  main : SIO ()
  main = return ()

  testList : FFI_Export FFI_Se "testHdr.se" []
  testList = Data Nat "Nat" $
             Data Bool "Bool" $
             Fun runDep "deposit" $
             Fun Bank.Main.runWith "withdraw" $
             End
