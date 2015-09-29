module Vindaloo where
import Data.Bool
import Data.Int
import qualified Data.ByteString as BS
import Data.Word
import Data.DoubleWord
import Data.ByteString

newtype Address = Address ByteString deriving (Show)
newtype SHAHash = SHAHash ByteString deriving (Show)

class EVMVar a where
  serialize :: a -> EVMVarT

class EVMVar a => Public a where
  expose :: a -> EVMVarT

data EVMVarT = ByteString ByteString
             | Bool Bool
             | Int8 Int8
             | UInt8 Word8 --[...]
             | Int256 Int256
             | UInt256 Word256 deriving (Show, Read, Eq, Ord)
