module Main

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
               (Yes p) => runInit [(),MkS prim__value prim__selfbalance 0 0, MkE prim__self prim__sender prim__origin] (playerChoice c {p})
               (No _)  => return False

runCheck : SIO Int
runCheck = case prim__value == 0 of
                True  => case (lte 20 prim__selfbalance) of
                              (Yes p) => runInit [(), MkS 0 prim__selfbalance 0 0] (check {p})
                              (No _)  => return 0
                False => return 0

exports : FFI_Export FFI_Se "defs.se" []
exports = Data Nat "Nat" $
          Data Bool "Bool" $
          Fun runInitC "init" $
          Fun runPC "playerChoice" $
          Fun runCheck "check" $
          End

