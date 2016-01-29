module Bank

import Ethereum

balances : MapField
balances = EMAddressInt "balances"

deposit : Eff ()
          [STORE, ETH v b 0 0, ENV c s o]
          [STORE, ETH v b 0 v, ENV c s o]
deposit {s} {v} = do
  update balances s (+ v)
  save v

withdraw : (a : Nat) -> Eff Bool
           [STORE, ETH 0 b 0 0, ENV c s o]
           (\success => if success
                           then [STORE, ETH 0 b a 0, ENV c s o]
                           else [STORE, ETH 0 b 0 0, ENV c s o])
withdraw a {s} = do
  if !(read balances s) >= a
     then do
       update balances s (\b => b - a)
       send a s
       pureM True
     else (pureM False)

