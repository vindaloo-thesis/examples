module GeneralStore

import Data.Vect
import Data.HVect

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

sign : Vect n Field -> Vect n Type
sign {n = S Z}     [f] = [HVect [interpField f] -> interpField f]
sign {n = S (S Z)} [f1,f2] = [
  (HVect [interpField f1, interpField f2] -> (interpField f1)),
  (HVect [interpField f1, interpField f2] -> (interpField f2))]

funcs :  (fs : Vect n Field) -> (( n < 2) = True) -> (if n == S Z then HVect [
          HVect [interpField (head fs)] -> interpField (head fs)
        ] else if n == S ( S Z) then HVect [
          (HVect [interpField (head fs), interpField (head (tail fs))] -> (interpField (head fs))),
          (HVect [interpField (head fs), interpField (head (tail fs))] -> (interpField (head (tail fs))))
        ])
funcs [_]    = [GeneralStore.head]
funcs [_, _] = [GeneralStore.head, index 1]
