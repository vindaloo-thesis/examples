module Main

import Decidable.Order
import Ethereum.EIO
import RPS

main : EIO ()
main = return ()

runInitC : EIO ()
runInitC = runInit [()] init

runPC : Int -> EIO Bool
runPC c = case lte 10 prim__value of
               Yes p => runInit
                            [(),MkEth prim__value prim__selfbalance 0 0, MkEnv prim__self prim__sender prim__origin]
                            (playerChoice c {p})
               No _  => return False

runCheck : EIO Int
runCheck = if prim__value == 0
              then case (lte 20 prim__selfbalance) of
                        (Yes p) => runInit [(), MkEth 0 prim__selfbalance 0 0] (check {p})
                        (No _)  => return 0
              else return 0

exports : FFI_Export FFI_Eth "defs.se" []
exports = Data Nat "Nat" $
          Data Bool "Bool" $
          Fun runInitC "init" $
          Fun runPC "playerChoice" $
          Fun runCheck "check" $
          End

