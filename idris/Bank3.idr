module Bank

import Ethereum

%default total

Owners : List Address
Owners = [0x00cf7667b8dd4ece1728ef7809bc844a1356aadf
         ,0x004a7617b84d4ece1728ef7809bc844356a897ba
         ]

deposit : Eff () [ETH v b 0 0] [ETH v b 0 v]
deposit {v} = save v

withdraw : (amount : Nat) -> {auto p: LTE amount b} -> Eff Bool
           [ETH 0 b 0 0, ENV c s o]
           (\success => if success
                           then [ETH 0 b amount 0, ENV c s o]
                           else [ETH 0 b 0      0, ENV c s o])
withdraw amount {s} = if s `elem` Owners 
                         then do
                           send amount s
                           pureM True
                         else pureM False

