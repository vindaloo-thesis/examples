module Bank

import Effects
import Ethereum
import Ethereum.SIO

deposit : TransEff.Eff () 
          [ETH v b 0 0]
          [ETH v b 0 v]
deposit {v} = save v

namespace Main
  runDep : SIO ()
  runDep = runInit [MkS prim__value 0 0 0] deposit

  main : SIO ()
  main = return ()
  
  testList : FFI_Export FFI_Se "testHdr.se" []
  testList = Data Nat "Nat" $
             Fun runDep "deposit" $
             End

