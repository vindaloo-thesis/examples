module Main

import Effects
import Effect.State
import Effect.StdIO
import Map
import Serialize
import Data.Vect

data Field = Int32 | EString | Map Field Field
data EVar = ES String | EI Int

interpField : Field -> Type
interpField Int32 = Int
interpField EString = String
interpField (Map x y) = Map (interpField x) (interpField y)

--Schema definition
Store : Type
Store = List (String, Field)

-- Hetereogenic list: parametized with a separate type for each entry. Effectively a Vect more than a list.
data HList : List Type -> Type where
  Nil : HList []
  (::) : t -> HList ts -> HList (t :: ts)

-- Interpretation function: takes Store and creates type
interp : Store -> Type
interp store = HList (map (interpField . snd) store)

-- The schema definition and field names (names probably not needed)
test : Store
test = [("t3xt", EString), ("num", Int32)]

-- Resulting type
itest : Type
itest = interp test

-- Example instantiated store
intest : HList [String, Int]
--intest : itest --this should work but doesn't...?
intest = ["hejje", 1337]


--Should give a list of functions to access fields from a given state
funcs : Store -> List ((HList a) -> EVar)
funcs store = (map ((\t =>
              case t of
                EString => \x => ES "x"
                Int32 => \x => EI 5)
            . snd) store)

main : IO ()
--main = putStrLn ((head (funcs store)) intest)
main = putStrLn "lol"


