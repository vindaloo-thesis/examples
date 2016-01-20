module Bank
import Effects
import Decidable.Order
import Ethereum
import Ethereum.Ether
import Ethereum.Environment
import Ethereum.SIO
import Ethereum.Types

%default total

namespace Bank2
  deposit : {v : Nat} -> Eff () [ETH v b]
  deposit {v} = return ()

  withdraw : (a : Nat) -> {b : Nat} -> {auto p: LTE a b} -> Eff Bool
             [ETH 0 b, ENV c 0x00cf7667b8dd4ece1728ef7809bc844a1356aadf o]
             (\success => if success
                             then [ETH 0 (b-a), ENV c 0x00cf7667b8dd4ece1728ef7809bc844a1356aadf o]
                             else [ETH 0 b,     ENV c 0x00cf7667b8dd4ece1728ef7809bc844a1356aadf o])
  withdraw a = if !(balance !contractAddress) >= a
                  then do
                    send a !sender
                    pureM True
                  else (pureM False)

namespace Main
  runDep : Nat -> SIO ()
  runDep v = runInit [MkS v 0] deposit

  runWith : Nat -> SIO Bool
  runWith a = case (lte a prim__balance) of
                   (Yes p) => case prim__value == 0 of
                                   True => runInit [MkS 0 prim__balance, MkE 0 0x00cf7667b8dd4ece1728ef7809bc844a1356aadf 0] (withdraw a {p})
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
