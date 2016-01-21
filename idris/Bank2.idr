module Bank
import Effects
import Decidable.Order
import Ethereum
import Ethereum.Ether
import Ethereum.Environment
import Ethereum.SIO
import Ethereum.Types

%default total

owner : Address
owner = 0x00cf7667b8dd4ece1728ef7809bc844a1356aadf

namespace Bank2
  deposit : {v : Nat} -> Eff () [ETH v b t s]
  deposit {v} = return ()

  withdraw : (a : Nat) -> {b : Nat} -> {auto p: LTE a b} -> Eff ()
             [ETH_IN 0 b, ENV c owner o]
             [ETH_OUT 0 b a 0, ENV c owner o]
  withdraw a = send a owner

namespace Main
  runDep : Nat -> SIO ()
  runDep v = runInit [MkS prim__value 0 0 0] deposit

  runWith : Nat -> SIO Bool
  runWith a = case (lte a prim__balance) of
                   (Yes p) => case prim__value == 0 of
                                   True => do
                                     runInit [MkS 0 prim__balance 0 0, MkE 0 0x00cf7667b8dd4ece1728ef7809bc844a1356aadf 0] (withdraw a {p})
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
