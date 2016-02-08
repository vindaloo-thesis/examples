module Main

import Effects
import Ethereum.EIO
import Bank

runDep : Maybe ()
runDep = runInit [MkEth prim__value 0 0 0] deposit

main : EIO ()
main = return ()

testList : FFI_Export FFI_Eth "testHdr.se" []
testList = Fun runDep "deposit" $
           End
