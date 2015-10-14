module Main

import Effects
import Effect.State
import Effect.StdIO
import Map
import Serialize
import Data.Vect
import Data.HVect

data Field = EInt | EString 
data EVar = ES String | EI Int

instance Show EVar where
  show (ES x) = x
  show (EI x) = show x

interpField : Field -> Type
interpField EInt = Int
interpField EString = String

--Schema definition
Store : Nat -> Type
Store k = Vect k Field

-- Interpretation function: takes Store and creates type
interp : Store k -> Type
interp store = HVect (map interpField store)

-- The schema definition and field names (names probably not needed)
test : Store 2
test = [EString, EInt]

-- Example instantiated store
intest : HVect [String, Int]
intest = ["hejje", 123]

head : HVect (t::ts) -> t
head [x] = x

tail : HVect (t::ts) -> HVect ts
tail (x::xs) = xs

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

funcs1 : (fs : Vect 1 Field) -> HVect [HVect [interpField (head fs)] -> interpField (head fs)]
funcs1 _    = [Main.head]

funcs2 : (fs : Vect 2 Field) -> HVect [
  (HVect [interpField (head fs), interpField (head (tail fs))] -> (interpField (head fs))),
  (HVect [interpField (head fs), interpField (head (tail fs))] -> (interpField (head (tail fs))))]
funcs2 _ = [Main.head, index 1]
 

main : IO ()
main = putStrLn (show ((head (tail (funcs2 test))) intest))
-- main = putStrLn "lol"


