module Main

import Decidable.Order
import Ethereum.EIO
import RPS

main : IO ()
main = return ()

runInitC : Maybe ()
runInitC = runInit [()] init

runJoinGame : String -> Maybe Bool
runJoinGame c = case lte 10 prim__value of
               Yes p => runInit
                            [(),MkEth prim__value prim__selfbalance 0 0, MkEnv prim__sender prim__origin]
                            (joinGame (MkCommit c) {p})
               No _  => Nothing

runFinalize : Maybe Int
runFinalize = if prim__value == 0
              then case (lte 20 prim__selfbalance) of
                        (Yes p) => runInit [(), MkEth 0 prim__selfbalance 0 0] (finalize {p})
                        (No _)  => return 0
              else Nothing

exports : FFI_Export FFI_Eth "defs.se" []
exports = Data Nat "Nat" $
          Data Bool "Bool" $
          Fun runInitC "init" $
          Fun runJoinGame "joinGame" $
          Fun runFinalize "finalize" $
          End

