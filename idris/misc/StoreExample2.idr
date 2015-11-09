module StoreExample2

import Effects
import Effect.StdIO
import Data.Vect
import Data.HVect

data Field = EInt | EString

Schema : Nat -> Type
Schema k = Vect k Field

InterpField : Field -> Type
InterpField (EInt) = Integer
InterpField (EString) = String

instance Show Field where
  show (EInt)     = "EINT_"
  show (EString)  = "ESTRING_"

deserialize : (f : Field) -> String -> InterpField f
deserialize (EInt)  = cast . prim__fromStrInt 
deserialize (EString) = id

serialize : (f : Field) -> InterpField f -> String
serialize (EInt )  = show
serialize (EString ) = id

-- Interpretation function: takes Schema and creates type
Interp : Schema k -> Type
Interp schema = HVect (map InterpField schema)

RPSStore : Schema 2 
RPSStore = [EInt, EInt]

Istore' : Interp RPSStore
Istore' = [0,0]

data Store : Effect where
  Read  : (f : Field) -> sig Store (InterpField f) (Interp s)
  Write : (f : Field) -> (InterpField f) -> sig Store () (Interp s)

STORE : (Schema k) -> EFFECT
STORE s = MkEff (Interp s) Store


read : (f : Field) -> Eff (InterpField f) [STORE s]
read f = call $ Read f

write : (f : Field) -> (InterpField f) -> SimpleEff.Eff () [STORE (s)]
write f x = call (Write f x)

instance Handler Store IO where
  handle s (Read field)     k =
    do
      Right val <- readFile (show field)
      k (deserialize field (trim val)) s
  handle s (Write field val) k =
    do
      writeFile (show field) (serialize field val)
      k () s


namespace TestContract
  updatePlayers :  {s: Schema k} -> SimpleEff.Eff Integer [STORE s] 
  updatePlayers  = do
		x <- read (EInt)
		write (EInt) (x+1)
		return x

namespace Main
  main : IO ()
  main = do
    x <- runInit [Istore'] (updatePlayers {s=RPSStore})
    putStrLn (show x)


