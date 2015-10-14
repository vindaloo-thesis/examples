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
Store k = Vect k (String, Field)

-- Interpretation function: takes Store and creates type
interp : Store k -> Type
interp store = HVect (map (interpField . snd) store)


-- The schema definition and field names (names probably not needed)
test : Store 2
test = [("t3xt", EString), ("num", EInt)]

-- Example instantiated store
intest : HVect [String, Int]
intest = ["hejje", 1337]

nap: Vect k a -> Vect k a
nap []    = []
nap (x::xs) = x::nap xs

--Should give a vector of functions to access fields from a given state
-- Given a Store, return a vector with functions to access each field in an instance of the store
-- funcs : Store k -> Vect k ((HVect {k = k} a) -> EVar)
funcs : {auto b: Type} -> (s: Vect n (String, Field)) -> Vect n ((interp s) -> b)
funcs store = map (\(i, t) =>
    case (snd t) of
         EString => \x => ((Data.HVect.index i x))
         EInt => \x => EI 5
  ) (zip range store)


{-
funcs : (s: Vect n (String, Field)) -> Vect n ((interp s) -> EVar)
funcs [] = []
funcs ((_,EInt) :: xs)    = (\x => EI 4)::funcs xs
funcs ((_,EString) :: xs) = (\x => ES "XX")::funcs xs
-}
{-
funcs : Vect n (String, Field) -> Vect n ((HVect {k = n} a) -> EVar)
funcs r@((_,EString) :: xs) = lookup where
  lookup : (interp r) -> EVar
  lookup x = case x of
                
  lookup  x    = ES "not found"

-}

main : IO ()
main = putStrLn (show ((Data.Vect.head (funcs test)) intest))
-- main = putStrLn "lol"


