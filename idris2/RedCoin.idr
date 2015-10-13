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

-- Resulting type
itest : Type
itest = interp test

-- Example instantiated store
intest : HVect [String, Int]
--intest : itest --this should work but doesn't...?
intest = ["hejje", 1337]


--Should give a list of functions to access fields from a given state
funcs : Store k -> Vect k ((HVect {k = k} a) -> EVar)
funcs store = (map (\(i, t) =>
              case (snd t) of
                EString => \x => Data.HVect.index i x  -- index (natToFin i) x
                -- EString => \x => (fromMaybe 1 (natToFin i k))  -- index (natToFin i) x
                Int32 => \x => EI 5)
             (zip range store))

--funcs2 : Store -> List ((HVect a) -> EVar) -- Nope. Can't infer a
-- funcs2 : Store -> List (a -> EVar) --sure, compiles, but no good
-- funcs2 store = (map ((\t => \x => ES "x"). snd) store)

main : IO ()
main = putStrLn (show ((head (funcs test)) intest))
-- main = putStrLn "lol"
-- main = putStrLn (show ((head (funcs2 test)) intest))


