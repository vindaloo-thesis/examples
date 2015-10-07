contract NameCoin where

import Control.Monad.State
import Control.Applicative
import Data.Word
import Data.Map as M

-------------------------------------------------------
-- Ethereum is a monad handling blockchain interaction.
-- It also maintains the contract's state.
data Ethereum s a

instance Functor (Ethereum s) where
  fmap = undefined

instance Applicative (Ethereum s) where
  pure  = undefined
  (<*>) = undefined

instance Monad (Ethereum s) where
  return = undefined
  (>>=)  = undefined

instance MonadState s (Ethereum s) where
  state = undefined
-------------------------------------------------------

-- The state is a registry, mapping Word32 to Word32.
type NameReg = Map Word32 Word32

{- VERSION 1:
This is very standard Haskell. The state is a monadic value, accessed
using 'get' and 'gets' as with all MonadState instances.
-}

register1 :: Word32 -> Word32 -> Ethereum NameReg Bool
register1 key val = do
    keyExists <- gets (M.member key)
    when (not keyExists) $ modify (M.insert key val)
    return (not keyExists)

get1 :: Word32 -> Ethereum NameReg Word32
get1 key = gets $ fromMaybe 0 . M.lookup key

{- VERSION 2:
This uses the function 'ifState', to allow the use of conditionals
on the state in a more functional way. 'ifState', or something similar,
would be part of the standard library. It *could* even be sugared away,
to look something like the code in version 3.
-}

register2 :: Word32 -> Word32 -> Ethereum NameReg Bool
register2 key val = ifState (M.member key)
                      (return False)
                      (return True <* modify (M.insert key val))
  where
    ifState p t f = do
        cond <- gets p
        if cond then t else f

get2 :: Word32 -> Ethereum NameReg Word32
get2 key = gets $ fromMaybe 0 . M.lookup key


{- VERSION 3:
This is not pure, or at least *very* sugared, syntax-wise. The identifyer
'state' represents the state at the time of the function call as a pure
value. For purity, it should be a monadic value which neither 'M.member'
nor 'M.lookup' can use as input.

However, we think that this gives developers a reasonable syntax to work
with. Breaking purity is not a very big deal since everyone knows that 
Ethereum is stateful. Additionally, this syntax would only be accessible
to functions already in the Ethereum monad; pure functions stay pure.

Additionally, the 'state' identifyer can't be used to *change* the state
in any way, only read it. All updates go through 'put' and 'modify', as
usual.
-}

register3 :: Word32 -> Word32 -> Ethereum NameReg Bool
register3 key val | key `M.member` state = return False
	        			  | otherwise            = return True <* modify (M.insert key val)

get3 :: Word32 -> Ethereum NameReg Word32
get3 key = return $ fromMaybe 0 $ M.lookup key state