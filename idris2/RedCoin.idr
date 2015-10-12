module Main

import Effects
import Effect.State
import Effect.StdIO
import Map
import Serialize
import Data.Vect

data Field = Int32 | EString | Map Field Field
data EVar = ES String

interpField : Field -> Type
interpField Int32 = Int
interpField EString = String
interpField (Map x y) = Map (interpField x) (interpField y)

Store : Type
Store = List (String, Field)

data HList : List Type -> Type where
  Nil : HList []
  (::) : t -> HList ts -> HList (t :: ts)

interp : Store -> Type
interp store = HList (map (interpField . snd) store)

test : Store
test = [("t3xt", EString), ("num", Int32)]

itest : Type
itest = interp test

intest : HList [String, Int]
intest = ["hejje", 1337]

store : Store
store = [("registry", Map EString Int32)]

--text : {t : Type}  -> String
--text {t=HList [String]} [x] = "x"
--num (x::y::[]) = y

funcs : Store -> List ((HList a) -> EVar)
funcs store = (map ((\t => case t of
              EString => \x => ES "x"
              Int32 => \x => ES "y"). snd) store)

reg : Map Nat Nat
reg = put 5 1338 (put 5 1337 Empty)

main : IO ()
main = putStrLn "lol"
--main = putStrLn (show (runPureInit [reg] (register 2 6)))


