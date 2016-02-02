module Main

import Bank4
import Ethereum.SIO

runDeposit : SIO ()
runDeposit = runInit [(), MkS prim__value prim__selfbalance 0 0, MkE 0x1 0x2 0x2] deposit

runWithdraw : Nat -> SIO Bool
runWithdraw amount = if prim__value == 0
                        then do
                          runInit
                            [(), MkS 0 prim__selfbalance 0 0, MkE 0 0x00cf7667b8dd4ece1728ef7809bc844a1356aadf 0]
                            (withdraw amount)
                          return True
                        else return False

main : IO ()
main = return ()

exports : FFI_Export FFI_Se "" []
exports = Data Nat "Nat" $
          Fun runDeposit "deposit" $
          Fun runWithdraw "withdraw" $
          End


