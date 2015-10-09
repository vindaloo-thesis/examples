module Main

import Effects
import Effect.StdIO

hello : { [STDIO] } Eff ()
hello = putStrLn "O hai"

main : IO ()
main = run hello
