module Name (state, fun1, fun2) where

state  = DB { players = 0 }

import Vindaloo
import Control.Monad.State.Lazy

data Contract  = DB { state :: DB, functions :: []}

data DB = DB { players :: (Player, Player)
             , num_players :: Int
             , reward :: Int
             , check_winner :: ((Bool, Bool, Bool), (Bool, Bool, Bool), (Bool, Bool, Bool))
             } deriving (Show)

data Player = Player { address :: Address
                     , commit :: SHAHash
                     , choice :: Choice
                     , has_revealed :: Bool
                     } deriving (Show)


data Choice = Rock | Paper | Scissors deriving (Show)

init :: () -> State DB
init = modify \db -> db { num_players = 0 }

player_input :: Choice -> State DB Int8
player_input hash = do


-- player_input :: SHAHash -> State DB RetVal
-- player_input hash = do
--   np <- gets num_players
--   return "hello"
