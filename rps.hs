import Vindaloo

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


init :: () -> DB
init = undefined

-- player_input :: SHAHash -> State DB RetVal
-- player_input hash = do
--   np <- gets num_players
--   return "hello"
