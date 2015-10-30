module GeneralStore

import Effects
import Effect.StdIO
import Data.Vect
import Data.HVect
import Data.Vect.Quantifiers
import Types


data Store : Effect where
  Read  : (f : Field) -> sig Store (interpField f) (Schema k)
  Write : (f : Field) -> (interpField f) -> sig Store () (Schema k)

STORE : Nat -> EFFECT
STORE k = MkEff (Schema k) Store

read : (f : Field) -> Eff (interpField f) [STORE ts]
read f = call $ Read f

write : (f : Field) -> (interpField f) -> Eff () [STORE ts]
write f x = call (Write f x)

deserialize : (f : Field) -> String -> interpField f
deserialize (EInt _)  = prim__fromStrInt 
deserialize (EString _) = id
deserialize (EAddress _) = id

serialize : (f : Field) -> interpField f -> String
serialize (EInt _)  = show
serialize (EString _) = id
serialize (EAddress _) = id

instance Handler Store IO where
  handle s (Read field)     k =
    do
      h <- openFile (show field) Read
      val <- fread h
      closeFile h
      k (deserialize field (trim val)) s

  handle s (Write field val) k =
    do
      h <- openFile (show field) Write
      fwrite h (serialize field val)
      closeFile h
      k () s










