-- I think lots of problems surfaced here... Will comment in more detail later.

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

init :: [Address] -> Int -> Ethereum TxState ()
init addrs n | length addrs < n = fail "fewer signers than required signatures"
             | otherwise        = S.put $ TxState { transaction = Nothing
                                                  , signers     = addrs
                                                  , minSigners  = n }

createTx :: Address -> Int -> Address -> Tx
createTx recepient a creator = Tx { to = recepient, amount = a, signedBy = [creator] }

send (minSigners,1) :: Address -> Int -> Ethereum TxState Int
send to amount = S.modify $ \s -> s{ transaction = Just (createTx to amount sender) }

sign (minSigners,1) :: Ethereum TxState ()
sign = S.modify $ \s -> s{ transaction = addSigner (transaction s) }
  where
    addSigner Nothing   = Nothing
    addSigner (Just tx) = let signedBy' = L.insert a (signedBy tx)
                           in Just tx{ signedBy = signedBy' }

finalize (minSigners,-minSigners) :: Ethereum TxState ()
finalize = do
    tx <- S.gets transaction
    required <- S.gets minSigners
    if length (signedBy tx) >= required
      then send (to tx) (amount tx) >> modify (\s -> s{transaction = Nothing})
      else fail "Transaction not signed by enough parties"

