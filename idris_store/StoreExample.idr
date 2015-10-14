module Main

import Data.HVect
import Store

-- TODO: Test if can assign functions like intended. try out namecoin example

-- The schema definition and field names 
test : Store 2
test = [EString, EInt]

-- Example instantiated store
intest : HVect [String, Int]
intest = ["hejje", 123]

main : IO ()
main = putStrLn (show ((head (funcs2 test)) intest))

