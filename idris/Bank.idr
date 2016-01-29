module Bank

import Effects
import Ethereum

deposit : TransEff.Eff () 
          [ETH v b 0 0]
          [ETH v b 0 v]
deposit {v} = save v

