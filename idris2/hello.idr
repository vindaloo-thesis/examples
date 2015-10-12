module Main

import Effects
import Builtins
import Effect.StdIO

hello : { [STDIO] } Eff ()
hello = putStrLn "O hai"

myPutStr : String -> IO ()
myPutStr x = foreign (FFun "putStr" [FString] FUnit) x

main : IO ()
main = run hello
