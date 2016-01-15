module Bank

import Effects
import Ethereum
import Ethereum.Types
import Ethereum.GeneralStore

--TODO: All addresses map to the same index :$
balances : MapField
balances = EMIntInt "balances"

namespace Bank
  deposit : {v : Nat} -> TransEff.Eff ()
            [ETH (Init v), STORE]
            [ETH (Running v 0 v), STORE]
  deposit {v} = do
    b <- read balances !sender
    write balances !sender (b+(toIntNat v))
    save v

  withdraw : (a : Nat) -> Eff Bool
             [ETH (Init 0), STORE]
             (\success => if success
                             then [ETH (Running 0 a 0), STORE]
                             else [ETH (Running 0 0 0), STORE])
  withdraw a = do
    b <- read balances !sender
    if b >= (toIntNat a)
       then do
         write balances !sender (b-(toIntNat a))
         send a !sender
         pureM True
       else (pureM False)

namespace Main
  main : IO ()
  main = return () {-do
    res <- runInit [MkS 0 0 0, ()] (withdraw 1)
    putStrLn . show $ res
    -}
  --main = runInit [MkS 10 0 0] (withdraw 0)


