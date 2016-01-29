module Bank

import Effects
import Decidable.Order
import Ethereum

%default total

Owner : Address
Owner = 0x00cf7667b8dd4ece1728ef7809bc844a1356aadf

deposit : Eff () [ETH v b 0 0] [ETH v b 0 v]
deposit {v} = save v

withdraw : (a : Nat) -> {auto p: LTE a b} -> Eff ()
           [ETH 0 b 0 0, ENV c Owner o]
           [ETH 0 b a 0, ENV c Owner o]
withdraw a = send a Owner

