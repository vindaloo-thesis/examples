module Main

import Bank4
import Ethereum.EIO

runDeposit : EIO ()
runDeposit = runInit [(), MkEth prim__value prim__selfbalance 0 0, MkEnv 0x2 0x2] deposit

runWithdraw : Nat -> EIO Bool
runWithdraw amount = if prim__value == 0
                        then do
                          runInit
                            [(), MkEth 0 prim__selfbalance 0 0, MkEnv 0x00cf7667b8dd4ece1728ef7809bc844a1356aadf 0]
                            (withdraw amount)
                          return True
                        else return False

main : IO ()
main = return ()

exports : FFI_Export FFI_Eth "" []
exports = Data Nat "Nat" $
          Fun runDeposit "deposit" $
          Fun runWithdraw "withdraw" $
          End


