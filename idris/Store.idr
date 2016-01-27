module Store

import Effects
import Ethereum
import Ethereum.IO

balances : Field
balances = EInt "balances"

namespace Bank
  deposit : Nat -> TransEff.Eff ()
            [STORE]
            [STORE]
  deposit v = do
    write (balances) v

namespace Main
  main : IO ()
  main = do
    res <- runInit [MkS 0 0 0, ()] (deposit 1)
    putStrLn . show $ res

