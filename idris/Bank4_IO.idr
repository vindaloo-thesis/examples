module Main

import Ethereum.IO
import Bank4

Alice : Address
Alice = 0x00cf7667b8dd4ece1728ef7809bc844a1356aadf

Bob : Address
Bob =   0x00144889afe235972acf978af842a1456eadf17a

main : IO ()
main = do
  runInit [(), MkS 10 0 0 0, MkE 0 Alice Alice] deposit
  ret <- runInit [(), MkS 0 10 0 0, MkE 0 Alice Alice] (withdraw 5)
  putStrLn $ "Alice withdraw 5: " ++ show ret
  --Unauthorized access
  ret <- runInit [(), MkS 0 5 0 0, MkE 0 Bob Bob] (withdraw 1)
  putStrLn $ "Bob withdraw 1: " ++ show ret
  ret <- runInit [(), MkS 0 5 0 0, MkE 0 Alice Alice] (withdraw 4)
  putStrLn $ "Alice withdraw 4: " ++ show ret
  ret <- runInit [(), MkS 0 1 0 0, MkE 0 Alice Alice] (withdraw 5)
  -- Overcharge
  putStrLn $ "Alice withdraw 5: " ++ show ret
  return ()


