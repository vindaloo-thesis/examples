module Main

import Effects
import Effect.State
import Effect.StdIO
import Map


record Store where
  constructor MkStore
  registry : Map Int String

reg : Map Nat Nat
reg = put 5 1338 (put 5 1337 Empty)

register :  Nat -> Nat -> Eff () [ETHEREUM (Store)]
register k v = update (put k v)


main : IO ()
main = putStrLn (show (runPureInit [reg] (register 2 6)))

