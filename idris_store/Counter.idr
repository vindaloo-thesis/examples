module Counter

import Effects
import Effect.State
import Effect.StdIO
import Data.HVect
import GeneralStore
import Control.IOExcept


store : Store 1
store = [EInt]

storet : Type
storet = interp store

istore : HVect [Int]
istore = [0]

namespace Contract
  Counter : Type -> Type
  Counter rTy = Eff rTy ['contState ::: STATE storet]

  increment : Counter ()
  increment = 'contState :- update (\[i] => [i+1]) 

  count : Counter Int
  count = pure (head !('contState :- get))

namespace User
  User : Type -> Type
  User rTy = Eff rTy ['contState ::: STATE (storet), STDIO]

myProg : User ()
myProg = do
  printLn !count
  increment
  increment
  printLn !count

instance Default t => Default (HVect [t]) where
  default = [default]



namespace Main
  main : IO ()
  main = run myProg

