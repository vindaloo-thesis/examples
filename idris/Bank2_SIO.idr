module Main

import Decidable.Order
import Effects
import Ethereum.SIO
import Bank2

runDeposit : SIO ()
runDeposit = runInit [MkS prim__value 0 0 0] deposit

runWithdraw : Nat -> SIO Bool
runWithdraw amount = case lte amount prim__selfbalance of
  Yes p => if prim__value == 0 
              then if prim__sender == Owner
                      then do
                        runInit
                          [MkS 0 prim__selfbalance 0 0, MkE prim__self Owner prim__origin]
                          (withdraw amount {p})
                        return True
                      else return False
              else return False
  No _  => return False

exports : FFI_Export FFI_Se "defs.se" []
exports = Data Nat "Nat" $
          Data Bool "Bool" $
          Fun runDeposit "deposit" $
          Fun runWithdraw "withdraw" $
          End

main : SIO ()
main = return ()
