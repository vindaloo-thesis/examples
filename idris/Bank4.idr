module Bank

import Ethereum

balances : Field Address Int
balances = MkField 0

deposit : Eff ()
          [STORE, ETH v b 0 0, ENV s o]
          [STORE, ETH v b 0 v, ENV s o]
deposit {s} {v} = do
  balance <- read balances s
  write balances s (balance + v)
  keep v

withdraw : (a : Nat) -> Eff Bool
           [STORE, ETH 0 b 0 0, ENV s o]
           (\success => if success
                           then [STORE, ETH 0 b a 0, ENV s o]
                           else [STORE, ETH 0 b 0 0, ENV s o])
withdraw a {s} = do
  balance <- read balances s
  if balance >= a
     then do
       write balances s (balance - a)
       send a s
       pureM True
     else (pureM False)

