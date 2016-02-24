module Main

import Effects
import Decidable.Order
import Ethereum.EIO
import Bank3

%default total

runDep : Nat -> EIO ()
runDep v = runInit [MkEth prim__value 0 0 0] deposit

runWithdraw : Nat -> EIO Bool
runWithdraw amount = case (lte amount prim__selfbalance) of
  (Yes p) => if prim__value == 0
                then do
                    runInit
                      [MkEth 0 prim__selfbalance 0 0, MkEnv 0x00cf7667b8dd4ece1728ef7809bc844a1356aadf 0]
                      (withdraw amount {p})
                    return True
                 else return False
  (No _)  => return False

main : EIO ()
main = return ()

exports : FFI_Export FFI_Eth "testHdr.se" []
exports = Data Nat "Nat" $
          Data Bool "Bool" $
          Fun runDep "deposit" $
          Fun runWithdraw "withdraw" $
          End

