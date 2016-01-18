module Bank

import Effects
import Ethereum
import Ethereum.Types

namespace Bank
  deposit : {v : Nat} -> TransEff.Eff () 
            [ETH (Init v)]
            [ETH (Running v 0 v)]
  deposit {v} = save v

namespace Main
  runDep : Nat -> SIO ()
  runDep v = runInit [MkS v 0 0] deposit

  main : SIO ()
  main = runDep 10
  
  testList : FFI_Export FFI_Se "testHdr.se" []
  testList = Data Nat "Nat" $
             Fun runDep "deposit" $
             End

