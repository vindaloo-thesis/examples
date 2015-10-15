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

funcs : (ts : Vect n Type) -> HVect (map (\t => HVect ts -> t) ts)
-- funcs : (fs: Store n) -> HVect (map (\t => interp fs -> t) (map interpField fs))
{-
funcs :  (fs : Vect n Field) -> {auto p : ( n < 3) = True} ->
                                
                                
                                (case n of 
         Z   => HVect []
         S Z => HVect [HVect [interpField (head fs)] -> interpField (head fs)]
         S (S Z) => HVect [
            (HVect [interpField (head fs), interpField (head (tail fs))] -> (interpField (head fs))),
            (HVect [interpField (head fs), interpField (head (tail fs))] -> (interpField (head (tail fs))))
          ]
         otherwise => HVect []
        )
-}
funcs []     = []
funcs [_]    = [GeneralStore.head]
funcs [_, _] = [GeneralStore.head, index 1]
