module Main

import Data.HVect
import Store

-- The schema definition and field names (names probably not needed)
test : Store 2
test = [EString, EInt]

-- Example instantiated store
intest : HVect [String, Int]
intest = ["hejje", 123]

main : IO ()
main = putStrLn (show ((head (funcs2 test)) intest))

