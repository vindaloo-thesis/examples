module Main

import Effects
import Ethereum.SIO
import Bank

runDep : SIO ()
runDep = runInit [MkS prim__value 0 0 0] deposit

main : SIO ()
main = return ()

testList : FFI_Export FFI_Se "testHdr.se" []
testList = Data Nat "Nat" $
           Fun runDep "deposit" $
           End
