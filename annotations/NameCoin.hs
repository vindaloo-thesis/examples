contract NameCoin 
( register
, get ) where

import Data.Map as M
import Data.Maybe as May
import Control.Monad.State as S

type Registry = Map Word32 Word32

-- There can be at most 2^32 keys registered at once, and the
-- function will increase this number by one if ot succeeds.
register (2^32,1) :: Word32 -> Word32 -> Ethereum Registry ()
register key val | key `member` state = fail "Existing key"
                 | otherwise          = S.modify (M.insert key val)



get (1,()) :: Word32 -> Ethereum Registry Word32
get key = S.gets $ May.fromMaybe 0 . M.lookup key
