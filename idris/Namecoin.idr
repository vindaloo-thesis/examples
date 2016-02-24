module Namecoin

import Ethereum

Owner : Address
Owner = 0x82a978b3f5962a5b0957d9ee9eef472ee55b42f1

db : Field Int Int
db = MkField 1

register : Int -> Int -> EthereumEff (res : ())
  { SENDER  = Owner
  ; ORIGIN  = origin
  }
register k v = write db k v

get : Int -> EthereumEff (val : Int) { }
get k = read db k

