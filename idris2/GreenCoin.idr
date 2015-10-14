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

--Should give a vector of functions to access fields from a given state
-- Given a Store, return a vector with functions to access each field in an instance of the store
-- funcs : Store k -> Vect k ((HVect {k = k} a) -> EVar)
-- funcs : (s: Vect n Field) -> Vect n ((interp s) -> )

funcs1 : (fs : Vect 1 Field) -> HVect [HVect [interpField (head fs)] -> EVar]
funcs1 [EInt]    = [\[x] => EI x]
funcs1 [EString] = [\[x] => ES x]

funcs2 : (fs : Vect 2 Field) -> HVect [HVect [interpField (head fs), interpField (head (tail fs))] -> EVar]
funcs2 [t1, t2] = [EI 4, EI 5]
{-
funcs2 [t1,t2] = [
  case t1 of
       EInt =>    (\[x,y] => EI x)
       EString => (\[x,y] => ES x)
  , case t2 of
       EInt    => (\[x,y] => EI y)
       EString => (\[x,y] => ES y)
  ]
-}
        
{-
  funcs1 : (fs' : Vect 1 Field) -> HVect [HVect [interpField (Data.Vect.head fs), interpField (Data.Vect.head (tail fs))] -> EVar]
  funcs1 [_, EInt] = [\(x::y::xs) => EI y]
  funcs1 [_, EString] = [\(x::y::xs) => ES y]
-}

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
main = putStrLn (show ((Main.head (funcs2 test)) intest))
-- main = putStrLn "lol"


