module Namecoin

import Ethereum

Owner : Address
Owner = 0x82a978b3f5962a5b0957d9ee9eef472ee55b42f1

db : Field Int Int
db = MkField 1

register : Int -> Int -> {auto p: LTE 10 value} -> EthereumEff (res : ())
  { SENDER  = sender
  ; ORIGIN  = Owner
  ; VALUE   = value
  ; BALANCE = balance
  ; TRANS   = 0
  ; KEEP    = 0
  }
register k v = write db k v

get : Int -> EthereumEff (val : Int) {}
get k = read db k

