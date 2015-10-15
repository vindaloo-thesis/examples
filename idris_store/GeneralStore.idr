module GeneralStore

import Data.Vect
import Data.HVect
import Data.Vect.Quantifiers

-- Missing head, tail function for HVect
head : HVect (t::ts) -> t
head (x::_) = x

tail : HVect (t::ts) -> HVect ts
tail (_::xs) = xs

data Field = EInt | EString 

interpField : Field -> Type
interpField EInt = Int
interpField EString = String

--Schema definition
Store : Nat -> Type
Store k = Vect k Field

-- Interpretation function: takes Store and creates type
interp : Store k -> Type
interp store = HVect (map interpField store)

--Courtesy of Melvar in #idris:

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

--funcs : (fs : Store n) -> HVect (map (\t => interp fs -> t) (map GeneralStore.interpField fs))
--funcs fs = allToHVect $ funcs' $ map interpField fs

funcs : (fs : Store n) -> HVect (map (\f => interp fs -> interpField f) fs)
funcs fs = rewrite sym $ mapMapMap (\t => interp fs -> t) interpField fs in allToHVect $ funcs' $ map interpField fs
