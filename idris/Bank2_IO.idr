module Main

import Ethereum.IO
import Bank2

main : IO ()
main = do
  runInit [MkEth 10 0 0 0] deposit
  runInit [MkEth 0 10 0 0, MkEnv Owner Owner] (withdraw 5)
  runInit [MkEth 0 5  0 0, MkEnv Owner Owner] (withdraw 1)
  -- Overdrawing account won't compile
  --runInit [MkEth 0 4 0 0, MkEnv 0x1 Owner Owner] (withdraw 5)
  -- Neither will incorrect sender
  --runInit [MkEth 0 5 0 0, MkEnv 0x1 deadbeef 0xdeadbeef] (withdraw 1)
