module Store

import Effects
import Ethereum
import Ethereum.Types
import Ethereum.GeneralStore

--TODO: All addresses map to the same index :$
balances : Field
balances = EInt "0"

namespace Bank
  deposit : Nat -> TransEff.Eff ()
            [STORE]
            [STORE]
  deposit v = do
    write (balances) (toIntNat v)


namespace Main
  main : IO ()
  main = do
    res <- runInit [MkS 0 0 0, ()] (deposit 1)
    putStrLn . show $ res
  --main = runInit [MkS 10 0 0] (withdraw 0)



