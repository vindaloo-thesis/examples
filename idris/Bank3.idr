module Bank

import Effects
import Ethereum
import Types
import GeneralStore

--TODO: All addresses map to the same index :$
balances : String -> Field
balances a = EInt 0 

namespace Bank
  deposit : {v : Nat} -> TransEff.Eff ()
            [ETHEREUM (Init v), STORE]
            [ETHEREUM (Running v 0 v), STORE]
  deposit {v} = do
    b <- read (balances !sender)
    write (balances !sender) (b+(toIntegerNat v))
    save v

  withdraw : (a : Nat) -> Eff Bool
             [ETHEREUM (Init 0), STORE]
             (\success => if success
                             then [ETHEREUM (Running 0 a 0), STORE]
                             else [ETHEREUM (Running 0 0 0), STORE])
  withdraw a = do
    b <- read (balances !sender)
    if b >= (toIntegerNat a)
       then do
         write (balances !sender) (b-(toIntegerNat a))
         send a !sender
         pureM True
       else (pureM False)

namespace Main
  main : IO ()
  main = do
    res <- runInit [MkS 0 0 0, ()] (withdraw 1)
    putStrLn . show $ res
  --main = runInit [MkS 10 0 0] (withdraw 0)


