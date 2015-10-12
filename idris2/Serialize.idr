module Serialize

class Serialize t where
  read : t -> String

instance Serialize Int where
  read x = show x
