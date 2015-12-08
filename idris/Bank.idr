module Bank

import Effects
import Ethereum
import Types

namespace Bank
  deposit : {v : Nat} -> TransEff.Eff () [ETHEREUM (Init v)] [ETHEREUM (Running v 0 v)]
  deposit {v} = do
    save v

namespace Main
  runDep : Nat -> IO ()
  runDep v = runInit [MkS v 0 0] deposit

  main : IO ()
  main = runDep 10
  
  
  testList : {x : Nat} -> FFI_Export FFI_C "testHdr.py" []
  testList {x} = Data Nat "Nat" $
             Data (List Nat) "ListNat" $
             --Data (TransEff.Eff () [ETHEREUM (Init x)] [ETHEREUM (Running x 0 x)]) "EffEth" $
             --Fun deposit "deposit" $
             Fun runDep "deposit" $
             End

