contract MultiSig
( send
, sign
, finalize ) where

import Control.Monad.State as S
import Data.List as L
import Data.Map as M
import Data.Maybe

data TxState = TxState { transaction :: Maybe Tx
                       , signers     :: [Address]
                       , minSigners  :: Int
                       }

data Tx = Tx { to       :: Address
             , amount   :: Int
             , signedBy :: [Address]
             }

init (0) :: [Address] -> Int -> Ethereum () TxState ()
init addrs n | length addrs < n = fail "fewer signers than required signatures"
             | otherwise        = S.put $ TxState { transaction = Nothing
                                                  , signers     = addrs
                                                  , minSigners  = n }

send ((),0,1) :: Address -> Int -> Ethereum () TxState ()
send to amount = S 