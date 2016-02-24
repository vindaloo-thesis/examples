module Main

import Decidable.Order
import Effects
import Ethereum.EIO
import Bank2

runDeposit : Maybe ()
runDeposit = runInit [MkEth prim__value 0 0 0] deposit

runWithdraw : Nat -> Maybe ()
runWithdraw amount = case lte amount prim__selfbalance of
  Yes p => if prim__value == 0 
              then if prim__sender == Owner
                      then runInit
                             [MkEth 0 prim__selfbalance 0 0
                              ,MkEnv Owner prim__origin]
                             (withdraw amount {p})
                      else Nothing
              else Nothing
  No _  => Nothing

exports : FFI_Export FFI_Eth "" []
exports = Fun runDeposit "deposit" $
          Fun runWithdraw "withdraw" $
          End

main : EIO ()
main = return ()
