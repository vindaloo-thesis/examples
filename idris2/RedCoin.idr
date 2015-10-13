module Main

import Effects
import Effect.State
import Effect.StdIO
import Map
import Serialize
import Data.Vect
import Data.HVect

data Field = Int32 | EString | Map Field Field
data EVar = ES String | EI Int

instance Show EVar where
  show (ES x) = x
  show (EI x) = show x

interpField : Field -> Type
interpField Int32 = Int
interpField EString = String
interpField (Map x y) = Map (interpField x) (interpField y)

--Schema definition
Store : Nat -> Type
Store k = Vect k (String, Field)

-- Interpretation function: takes Store and creates type
interp : Store k -> Type
interp store = HVect (map (interpField . snd) store)

-- The schema definition and field names (names probably not needed)
test : Store 2
test = [("t3xt", EString), ("num", Int32)]

-- Example instantiated store
intest : HVect [String, Int]
intest = ["hejje", 1337]

-- Attempt to write cast instance, maybe this is the wrong way to go and funcs should have an elaborate type signature
instance Cast (Data.HVect.index i a) String where
  cast  x = "x"

--Should give a vector of functions to access fields from a given state
-- Given a Store, return a vector with functions to access each field in an instance of the store
funcs : Store k -> Vect k ((HVect {k = k} a) -> EVar)
funcs store = map (\(i, t) =>
    case (snd t) of
         EString => \x => ES (cast (Data.HVect.index i x))
         Int32 => \x => EI 5
  ) (zip range store)

main : IO ()
main = putStrLn (show ((head (funcs test)) intest))
-- main = putStrLn "lol"


