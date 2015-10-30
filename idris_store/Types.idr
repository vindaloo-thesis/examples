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
data Field = EInt Nat | EString Nat | EAddress Nat 

instance Show Field where
  show (EInt n)     = "EINT_" ++ show n
  show (EString n)  = "ESTRING_" ++ show n
  show (EAddress n) = "EADDRESS_" ++ show n

--Schema definition
Schema : Nat -> Type
Schema k = Vect k Field

interpField : Field -> Type
interpField (EInt _) = Int
interpField (EString _) = String
interpField (EAddress _) = String

-- Interpretation function: takes Schema and creates type
interp : Schema k -> Type
interp schema = HVect (map interpField schema)







------------FUNCS-------------

extends : All (\t => HVect ts -> t) us -> All (\t => HVect (u :: ts) -> t) us
extends []        = []
extends (f :: fs) = (f . tail) :: extends fs

funcs' : (ts : Vect n Type) -> All (\t => HVect ts -> t) ts
funcs' []        = []
funcs' (x :: xs) = head :: extends (funcs' xs)

allToHVect : All p xs -> HVect (map p xs)
allToHVect []        = []
allToHVect (x :: xs) = x :: allToHVect xs

mapMapMap : (f : b -> c) -> (g : a -> b) -> (xs : Vect n a) -> map f (map g xs) = map (f . g) xs
mapMapMap f g []        = Refl
mapMapMap f g (x :: xs) = cong $ mapMapMap f g xs

funcs : (fs : Schema n) -> HVect (map (\f => interp fs -> interpField f) fs)
funcs fs = rewrite sym $ mapMapMap (\t => interp fs -> t) interpField fs in allToHVect $ funcs' $ map interpField fs
