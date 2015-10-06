-- Nothing interesting happens here. No annotations, no anything.
-- NameCoin is just too simple. :(

contract NameCoin 
( register
, get ) where

import Data.Map as M
import Data.Maybe as May
import Control.Monad.State as S

type Registry = Map Word32 Word32

register :: Word32 -> Word32 -> Ethereum Registry ()
register key val | key `member` state = fail "Existing key"
                 | otherwise          = S.modify (M.insert key val)



get :: Word32 -> Ethereum Registry Word32
get key = S.gets $ May.fromMaybe 0 . M.lookup key
