module Main

import Decidable.Order
import Ethereum.EIO
import RPS

main : IO ()
main = return ()

runInitC : Maybe ()
runInitC = runInit [()] init

runPC : Int -> Maybe Bool
runPC c = case lte 10 prim__value of
               Yes p => runInit
                            [(),MkEth prim__value prim__selfbalance 0 0, MkEnv prim__self prim__sender prim__origin]
                            (playerChoice c {p})
               No _  => Nothing

runCheck : Maybe Int
runCheck = if prim__value == 0
              then case (lte 20 prim__selfbalance) of
                        (Yes p) => runInit [(), MkEth 0 prim__selfbalance 0 0] (check {p})
                        (No _)  => return 0
              else Nothing

exports : FFI_Export FFI_Eth "defs.se" []
exports = Data Nat "Nat" $
          Data Bool "Bool" $
          Fun runInitC "init" $
          Fun runPC "playerChoice" $
          Fun runCheck "check" $
          End

