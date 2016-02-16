module Main

import Effects
import Ethereum.EIO
import Namecoin

runReg : Int -> Int -> Maybe ()
runReg k v = if prim__origin == Owner
				then runInit [(), MkEnv prim__self prim__sender Owner] (register k v)
				else Nothing

runGet : Int -> Maybe Int
runGet k = runInit [()] (get k)

main : EIO ()
main = return ()

testList : FFI_Export FFI_Eth "" []
testList = Fun runReg "register" $
           Fun runGet "get" $
           End

