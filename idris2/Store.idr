module Store

import Data.Vect
import Data.HVect

-- Missing head, tail function for HVect
head : HVect (t::ts) -> t
head [x] = x

tail : HVect (t::ts) -> HVect ts
tail (x::xs) = xs

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

funcs1 : (fs : Vect 1 Field) -> HVect [HVect [interpField (head fs)] -> interpField (head fs)]
funcs1 _    = [head]

funcs2 : (fs : Vect 2 Field) -> HVect [
  (HVect [interpField (head fs), interpField (head (tail fs))] -> (interpField (head fs))),
  (HVect [interpField (head fs), interpField (head (tail fs))] -> (interpField (head (tail fs))))]
funcs2 _ = [head, index 1]

{- Failed unification attempt
sign : Vect n Field -> Vect n Type
sign {n = S Z}     [f] = [HVect [interpField f] -> interpField f]
sign {n = S (S Z)} [f1,f2] = [
  (HVect [interpField f1, interpField f2] -> (interpField f1)),
  (HVect [interpField f1, interpField f2] -> (interpField f2))]

funcs : (fs : Vect n Field) -> HVect (sign fs)
funcs [_]    = [Main.head]
funcs [_, _] = [Main.head, index 1]
-}
