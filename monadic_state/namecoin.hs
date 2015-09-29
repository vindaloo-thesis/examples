module Namecoin where
import Vindaloo
import qualified Control.Monad.State.Lazy as S
import qualified Data.Map as M

ifState = undefined
type DB = M.Map EVMVarT EVMVarT

register :: (Public a, Public b) => a -> b -> S.State DB Bool
register key val = do st <- S.get
                      if (M.member (expose key) st) then return False
                                           else do S.modify $ M.insert (expose key) (expose val)
                                                   return True


get :: (Public a) => a -> S.State DB EVMVarT
get key = do st <- S.get
             case (M.lookup (expose key) st) of
               Just x -> return x
               Nothing -> return $ Int8 1

