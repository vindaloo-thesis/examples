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

{- The information on minSigners is insuffient.

In the RPS example, it specified the maximum allowed number of participants.
Here, for send and sign, it shows the maximum allowed value for the variable.

For finalize, it shows the minimum number of participants required before the
function can be called.
-}
send ((),1) :: Address -> Int -> Ethereum TxState Int
send to amount = S.modify $ \s -> s{ transaction = Just (createTx to amount sender) }

sign ((),1) :: Ethereum TxState ()
sign = S.modify $ \s -> s{ transaction = addSigner (transaction s) } -- we reeeeeally need better syntax for updating the state. make (a subset of) lenses part of the stdlib?
  where
    addSigner Nothing   = Nothing
    addSigner (Just tx) = let signedBy' = L.insert a (signedBy tx)
                           in Just tx{ signedBy = signedBy' }

finalize (minSigners,-minSigners) :: Ethereum TxState ()
finalize = do
    let Just tx = transaction state -- this is bad syntax imo. state is so central to contracts, so its components should be directly accessible (for reading, not writing!) from all stateful functions
    send (to tx) (amount tx)
    modify (\s -> s{transaction = Nothing})

