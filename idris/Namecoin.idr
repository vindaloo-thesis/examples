module Namecoin

import Ethereum

Owner : Address
Owner = 0x82a978b3f5962a5b0957d9ee9eef472ee55b42f1

db : Field Int Int
db = MkField 1

register : Int -> Int -> Eff () [STORE, ENV self sender Owner]
register k v = write db k v

get : Int -> Eff Int [STORE]
get k = read db k

