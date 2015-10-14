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
test : Store 1
test = [EString]

-- Example instantiated store
intest : HVect [String]
intest = ["hejje"]

--Should give a vector of functions to access fields from a given state
-- Given a Store, return a vector with functions to access each field in an instance of the store
-- funcs : Store k -> Vect k ((HVect {k = k} a) -> EVar)
-- funcs : (s: Vect n Field) -> Vect n ((interp s) -> )

funcs1 : (fs : Vect 1 Field) -> HVect [HVect [interpField (head fs)] -> EVar]
funcs1 [EInt] = [\(x::xs) => EI x]
funcs1 [EString] = [\(x::xs) => ES x]

-- funcs2 : ((f1:: : Vect 2 Field) -> HVect [(interpField f1 -> EVar)::(interpField f2 -> EVar)]
-- funcs2 (EInt::rs) = EI :: funcs1 rs
{-
funcs : (s: Vect n (String, Field)) -> Vect n ((interp s) -> EVar)
funcs [] = []
funcs ((_,EInt) :: xs)    = (\x => EI 4)::funcs xs
funcs ((_,EString) :: xs) = (\x => ES "XX")::funcs xs
-}

{-
funcs : (s: Vect n Type) -> Vect n ((HVect {k = n} a) -> EVar)
funcs r@((_,EString) :: xs) = lookup where
  lookup : (interp r) -> EVar
  lookup x = case x of
                
  lookup  x    = ES "not found"

-}

head : HVect (t::ts) -> t
head (x::xs) = x

main : IO ()
main = putStrLn (show ((Main.head (funcs1 test)) intest))
-- main = putStrLn "lol"


