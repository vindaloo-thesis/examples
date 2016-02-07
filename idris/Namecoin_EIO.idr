module Main

import Effects
import Ethereum.EIO
import Namecoin

runReg : Int -> Int -> Maybe ()
runReg k v = runInit [()] (register k v)

runGet : Int -> Maybe Int
runGet k = runInit [()] (get k)

main : EIO ()
main = return ()

testList : FFI_Export FFI_Eth "" []
testList = Fun runReg "register" $
           Fun runGet "get" $
           End

