module Main

import Ethereum.IO
import Bank

main : IO ()
main = runInit [MkS 10 0 0 0] deposit

