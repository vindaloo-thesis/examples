module Main

import Effects
import Effect.State
import Effect.StdIO
import Map


data Field = Int32 | EString | Map Field Field

interpField : Field -> Type
interpField Int32 = Nat
interpField EString = String
interpField (Map x y) = Map (interpField x) (interpField y)

Store : Type
Store = List (String, Field)

data HList : List Type -> Type where
  Nil : HList []
  (::) : t -> HList ts -> HList (t :: ts)

interp : Store -> Type
interp store = HList (map (interpField . snd) store)

-- tag code. move label to type level
data Section : String -> Type -> Type where
  MkSection : (lbl : String) -> (x : a) -> Section lbl a
--snip

test : Store
test = [("text", EString), ("num", Int32)]

store : Store
store = [("registry", Map EString Int32)]

text : HList [String, Int] -> String
num : HList [String, Int] -> Int
text (x::y::[]) = x
num (x::y::[]) = y

derp2 : Type -> Nat
derp2 (HList (Int::Nil)) = 1

reg : Map Nat Nat
reg = put 5 1338 (put 5 1337 Empty)

-- register :  Nat -> Nat -> Eff () [ETHEREUM (Store)]
-- register k v = update (put k v)


main : IO ()
main = putStrLn "lol"
--main = putStrLn (show (runPureInit [reg] (register 2 6)))

