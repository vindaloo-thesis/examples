module Main

import Effects
import Effect.State
import Map

record Store where
  constructor MkStore
  registry : Map Int String

reg : Map Nat Nat
reg = put 5 1338 (put 5 1337 Empty)

register :  Nat -> Nat -> Eff () [STATE (Map Nat Nat)]
register k v
  = do old <- get
       put (put k v old)


main : IO ()
main = putStrLn (show (runPureInit [reg] (register 2 6)))

