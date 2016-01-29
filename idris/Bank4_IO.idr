module Main

import Ethereum.IO
import Bank5

main : IO ()
main = do
  runInit [(), MkS 10 0 0 0, MkE 0 0x00cf7667b8dd4ece1728ef7809bc844a1356aadf 0x00cf7667b8dd4ece1728ef7809bc844a1356aadf] deposit
  ret <- runInit [(), MkS 0 10 0 0, MkE 0 0x00cf7667b8dd4ece1728ef7809bc844a1356aadf 0] (withdraw 5)
  ret <- runInit [(), MkS 0 5 0 0, MkE 0 0x004a7617b84d4ece1728ef7809bc844356a897ba 0] (withdraw 1)
  --Overdrawing account won't work
  --ret <- runInit [MkS 0 4 0 0, MkE 0 0x00cf7667b8dd4ece1728ef7809bc844a1356aadf 0] (withdraw 5)
  return ()


