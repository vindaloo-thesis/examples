module Main

import Decidable.Order
import Effects
import Ethereum.SIO
import Bank2

runDeposit : SIO ()
runDeposit = runInit [MkS prim__value 0 0 0] deposit

runWithdraw : Nat -> SIO Bool
runWithdraw a = case lte a prim__selfbalance of
                     Yes p => case prim__value == 0 of
                                     True => do
                                       runInit [MkS 0 prim__selfbalance 0 0, MkE 0 0x00cf7667b8dd4ece1728ef7809bc844a1356aadf 0] (withdraw a {p})
                                       return True
                                     False => return False
                     No _  => return False

exports : FFI_Export FFI_Se "defs.se" []
exports = Data Nat "Nat" $
          Data Bool "Bool" $
          Fun runDeposit "deposit" $
          Fun runWithdraw "withdraw" $
          End

main : SIO ()
main = return ()
