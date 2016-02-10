module Bank

import Ethereum

%default total

Owner : Address
Owner = 0xead301dc6b949faf4a5c4a14174611574a884845

deposit : Eff () [ETH v b 0 0] [ETH v b 0 v]
deposit {v} = keep v

withdraw : (amount : Nat) -> {auto p: LTE amount b} -> Eff ()
           [ETH 0 b 0 0, ENV c Owner o]
           [ETH 0 b amount 0, ENV c Owner o]
withdraw amount = send amount Owner

