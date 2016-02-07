module Main

import Ethereum.IO
import Bank2

main : IO ()
main = do
  runInit [MkEth 10 0 0 0] (deposit)
  ret <- runInit [MkEth 0 10 0 0, MkEnv 0 0x00cf7667b8dd4ece1728ef7809bc844a1356aadf 0] (withdraw 5)
  ret <- runInit [MkEth 0 5 0 0, MkEnv 0 0x00cf7667b8dd4ece1728ef7809bc844a1356aadf 0] (withdraw 1)
  --Overdrawing account won't work
  --ret <- runInit [MkEth 0 4 0 0, MkEnv 0 0x00cf7667b8dd4ece1728ef7809bc844a1356aadf 0] (withdraw 5)
  return ()
