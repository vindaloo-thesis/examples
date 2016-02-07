module Main

import Effects
import Ethereum.EIO
import Mul

runMul2 : Int -> Maybe Int
runMul2 a = runInit [] (mul2 a)

main : EIO ()
main = return ()

testList : FFI_Export FFI_Eth "testHdr.se" []
testList = Data Int "Int" $
           Data (Maybe Int) "M" $
           Fun runMul2 "mul2" $
           End

