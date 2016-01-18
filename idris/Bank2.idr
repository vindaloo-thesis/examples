module Bank
import Effects
import Ethereum
import Ethereum.Ether
import Ethereum.SIO
import Ethereum.Types

namespace Bank2
  deposit : {v : Nat} -> Eff ()
            [ETH (Init v)]
            [ETH (Running v 0 v)]
  deposit {v} = save v

  withdraw : (a : Nat) -> Eff Bool
             [ETH (Init 0)]
             (\success => if success
                             then [ETH (Running 0 a 0)]
                             else [ETH (Running 0 0 0)])
  withdraw a = if !sender ==
                  0x00cf7667b8dd4ece1728ef7809bc844a1356aadf
                  && !(balance !contractAddress) >= a
                  then do
                    send a !sender
                    pureM True
                  else (pureM False)

namespace Main
  runDep : Nat -> SIO ()
  runDep v = runInit [MkS v 0 0] deposit

  runWith : Nat -> SIO Bool
  runWith v = runInit [MkS 0 0 0] (withdraw v)

  main : SIO ()
  main = return ()

  testList : FFI_Export FFI_Se "testHdr.se" []
  testList = Data Nat "Nat" $
             Data Bool "Bool" $
             Fun runDep "deposit" $
             Fun Bank.Main.runWith "withdraw" $
             End
