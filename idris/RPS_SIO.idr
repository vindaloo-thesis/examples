module RPS_SIO

import Decidable.Order
import Effects
import Ethereum
import Ethereum.SIO
import RPS

main : SIO ()
main = return ()

runInitC : SIO ()
runInitC = runInit [()] init

runPC : Int -> SIO Bool
runPC c = case (lte 10 prim__value) of
               (Yes p) => runInit [(),MkS prim__value prim__selfbalance 0 0] (playerChoice c {p})
               (No _)  => return False

runCheck : SIO Int
runCheck = case prim__value == 0 of
                True  => runInit [(), MkS 0 prim__selfbalance 0 0] check
                False => return 0

testList : FFI_Export FFI_Se "testHdr.se" []
testList = Data Nat "Nat" $
           Data Bool "Bool" $
           Fun runInitC "init" $
           Fun runPC "playerChoice" $
           Fun runCheck "check" $
           End

