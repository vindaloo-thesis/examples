module Main

import Effects
import Effect.State

register :  Nat -> Eff Nat [STATE (List Nat)]
register x = do update (x::); pure (length !get)

main : IO ()
main = putStrLn (show (runPureInit [[128]] (register 2)))
