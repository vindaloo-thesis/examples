-- I think lots of problems surfaced here... Will comment in more detail later.

contract MultiSig
( createTx
, newTransaction ) where

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
             , verified :: Bool
             }

init :: [Address] -> Int -> Ethereum TxState ()
init addrs n | length addrs < n = fail "fewer signers than required signatures"
             | otherwise        = S.put $ TxState { transaction = Nothing
                                                  , signers     = addrs
                                                  , minSigners  = n }

createTx :: Address -> Int -> Address -> Tx
createTx a i v = Tx { to = a, amount = i, signedBy = [v], verified = False }

newTransaction (minSigners,1) :: Address -> Int -> Ethereum TxState Int
newTransaction to amount = S.modify $ \s -> s{ transaction = Just (createTx to amount sender) }

signTx (minSigners,1) :: Ethereum TxState ()
signTx = S.modify $ \s -> s{ transaction = addSigner (transaction s) }
  where
    addSigner Nothing   = Nothing
    addSigner (Just tx) = let signedBy' = L.insert a (signedBy tx)
                           in Just tx{ signedBy = signedBy'
                                     , verified = length signedBy >= minSigners }

finalize (minSigners,-minSigners) :: Ethereum TxState ()
finalize = do
    tx <- S.gets $ transaction
    if verified tx
      then send (to tx) (amount tx) >> modify (\s -> s{transaction = Nothing})
      else fail "Transaction not signed by enough parties"

