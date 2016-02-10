module Bank

import Ethereum

deposit : Eff () 
          [ETH v b 0 0]
          [ETH v b 0 v]
deposit {v} = keep v

