import Data.Bool
import Data.Int
import qualified Data.ByteString as BS
import Data.Word
import Data.DoubleWord
import Data.ByteString

newtype Address = Address String deriving (Show)
newtype SHAHash = SHAHash String deriving (Show)

class EVMVar a where
  serialize :: a -> EVMVarT

data EVMVarT = ByteString ByteString
             | Bool Bool
             | Int8 Int8
             | UInt8 Word8 --[...]
             | Int256 Int256
             | UInt256 Word256
