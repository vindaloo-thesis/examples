module Main

import Effects
import Decidable.Order
import Ethereum.SIO
import Bank3

%default total

runDep : Nat -> SIO ()
runDep v = runInit [MkS prim__value 0 0 0] deposit

runWithdraw : Nat -> SIO Bool
runWithdraw amount = case (lte amount prim__selfbalance) of
  (Yes p) => if prim__value == 0
                then do
                    runInit
                      [MkS 0 prim__selfbalance 0 0, MkE 0 0x00cf7667b8dd4ece1728ef7809bc844a1356aadf 0]
                      (withdraw amount {p})
                    return True
                 else return False
  (No _)  => return False

main : SIO ()
main = return ()

exports : FFI_Export FFI_Se "testHdr.se" []
exports = Data Nat "Nat" $
          Data Bool "Bool" $
          Fun runDep "deposit" $
          Fun runWithdraw "withdraw" $
          End

