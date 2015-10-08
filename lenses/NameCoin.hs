contract NameCoin
( register
, get ) where

import Control.Lens.Setter ((%=),(^.),(.=))
import Data.Map (lookup,member,empty)
import Data.Maybe (fromMaybe)

-- The state is declared as a special data structure. It is an implicit
-- parameter to the Ethereum monad.
state = { registry :: Map Word32 Word32 }

-- (.=) is an operator for setting the value a lens is focused on.
-- Lenses are basically abstractions of getters and setters. See next
-- comment for more info.
init :: Ethereum ()
init = registry .= empty

{- Two assumptions are made below:

1. 'state' is the current value of the state. (Regular value, not monadic!)
   It has the type 'State'.
2. A lens has automatically been generated (there exist functions for doing
   that) for all fields in the state, with the same name as the field has in
   the source code. Such a name clash is not allowed, but possible to sugar
   away *if* we rename the fields themselves to something else, e.g. '_registry'.
   
   To be clear:
   registry  :: Lens State (Map Word32 Word32)
   _registry :: State -> Map Word32 Word32
   
   This might not be worth it.

(^.) is an operator for accessing the value a lens is focused on from a
data structure. In this case, it simply retrieves the only value inside
the data structure, namely the actual mapping.

(%=) is an operator for modifying the value a lens is focused on using a
function, when it is in a stateful monad. In this case, the supplied function
inserts the supplied value at the supplied key.
-}
register :: Word32 -> Word32 -> Ethereum ()
register key val | key `member` state^.registry = fail "Key taken"
                 | otherwise                    = registry %= insert key val

-- Nothing weird here if you understood the previous function. :)
get :: Word32 -> Ethereum Word32
get key = return $ fromMaybe 0 $ lookup key (state^.registry)