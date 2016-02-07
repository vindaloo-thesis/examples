module Mul

import Ethereum

mul2 : Int -> Eff Int
          []
mul2 a = return (a*2)


{-
mulval : Int -> Eff Int
          [ETH v b 0 0]
          [ETH v b 0 v]
mulval {v} a = do
  save v
  return (a*v)
  -}
