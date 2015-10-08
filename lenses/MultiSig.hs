contract MultiSig
( send
, sign
, finalize ) where

import Control.Lens ((^.),(.=),(%=))
import Control.Monad (when)
import Data.Map (empty,lookup,adjustWithKey) as M
import Data.Maybe (isNothing,fromJust)
import Data.Set as S

data Tx = Tx { recipient :: Address
             , amount    :: Int
             , signedBy  :: Set Address
             }

type TxId = Int

state = { txs         :: Map TxId Tx
        , signers     :: [Address]
        , minSigners  :: Int
        }

init :: [Address] -> Int -> Ethereum ()
init addrs n = do
    txs        .= empty
    signers    .= addrs
    minSigners .= n

send :: Address -> Int -> Ethereum TxId
send to n | sender `elem` state^.signers = do
    txid <- newRef
    txs %= M.insert txid (newTx to n sender)
    return txid

sign :: TxId -> Ethereum ()
sign txid | txid `member` state^.txs && sender `elem` state^.signers = do
                let Just tx = lookup txid (state^.txs)
                txs %= adjustWithKey addSigner txid
          | otherwise = fail "No such tx" 

finalize :: TxId -> Ethereum ()
finalize txid = do
    let tx = lookup txid (state^.txs)
    when (isNothing tx) (fail "No such tx")
    let nSigners = size $ signedBy $ fromJust tx
    unless (nSigners < state^.minSigners) $ do
      send (recipient tx) (amount tx)
      txs %= delete txid

newTx :: Address -> Int -> Address -> Tx
newTx to n creator = Tx { recipient to, amount = n, signedBy = [creator] }

addSigner :: Address -> Tx -> Tx
addSigner signer tx = tx { signedBy = S.insert signer (signedBy tx) }