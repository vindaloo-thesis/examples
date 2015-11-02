module Types

import Data.Vect
import Data.HVect
import Data.Vect.Quantifiers

-- Missing head, tail function for HVect
head : HVect (t::ts) -> t
head (x::_) = x

tail : HVect (t::ts) -> HVect ts
tail (_::xs) = xs

-- TODO: Lists, maps
-- parameter: index
data Field = EInt Nat | EString Nat | EAddress Nat | EArray Nat

instance Show Field where
  show (EInt n)     = "EINT_" ++ show n
  show (EString n)  = "ESTRING_" ++ show n
  show (EAddress n) = "EADDRESS_" ++ show n

--Schema definition
Schema : Nat -> Type
Schema k = Vect k Field

InterpField : Field -> Type
InterpField (EInt _) = Integer
InterpField (EString _) = String
InterpField (EAddress _) = String

-- Interpretation function: takes Schema and creates type
Interp : Schema k -> Type
Interp schema = HVect (map InterpField schema)

