module RPS_IO

import Effects
import Ethereum
import Ethereum.IO
import RPS

namespace Main
  main : IO ()
  main = do
    runInit [()] (init)
    res <- runInit [(), MkS 10 0 0] (playerChoice 0)
    putStrLn . show $ res

