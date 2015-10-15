module Main

import Data.HVect
import GeneralStore
--import Store

-- TODO: Test if can assign functions like intended. try out namecoin example

-- The schema definition and field names 
store0 : Store 0
store0 = []

store1 : Store 1
store1 = [EString]

store2 : Store 2
store2 = [EString, EInt]

-- Example instantiated store
istore0 : HVect []
istore0 = []

istore1 : HVect [String]
istore1 = ["hejje"]

istore2 : HVect [String, Int]
istore2 = ["hejje", 123]

main : IO ()
--main = putStrLn (show (GeneralStore.head ((funcs store1 Refl)) istore1))
main = putStrLn (show (funcs store0))
--main = putStrLn (show (funcs store0 Refl))
--main = putStrLn (show (funcs store0 Refl))

