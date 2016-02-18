module Main

import Decidable.Order
import Effects
import Ethereum.EIO
import Namecoin

runRegister : Int -> Int -> Maybe ()
runRegister k v = if prim__sender == Owner 
				then runInit [MkEth prim__value prim__selfbalance 0 0, (), MkEnv prim__self Owner prim__origin] (register k v)
				else Nothing

runGet : Int -> Maybe Int
runGet k = runInit [MkEth prim__value prim__selfbalance 0 0, (), MkEnv prim__self prim__sender prim__origin] (get k)

main : EIO ()
main = return ()

testList : FFI_Export FFI_Eth "" []
testList = Fun runRegister "register" $
           Fun runGet "get" $
           End

