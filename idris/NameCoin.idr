module NameCoin

state : { registry : Map Word32 Word32 }

public
register : Word32 -> Word32 -> Ethereum ()
register key val | key `notMember` !registry = [| (insert key val) registry |]

public
get : Word32 -> Ethereum Word32
get key = [| (fromMaybe 0 . lookup key) registry |]