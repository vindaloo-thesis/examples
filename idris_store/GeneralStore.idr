module GeneralStore

import Effects
import Effect.StdIO
import Data.Vect
import Data.HVect
import Data.Vect.Quantifiers
import Types


data Store : Effect where
  Read  : (f: Field) -> NoUpdateEffect.sig Store (interpField f) (interp ts)
--  Write : Show t => Field t -> t -> sig Store () (Environment ts)

STORE : (Schema k) -> EFFECT
STORE ts = MkEff (interp ts) Store

read : (f : Field) -> Eff (interpField f) [STORE ts]
read f = call $ Read f

--write : Show t => Ident t -> t -> Eff () [STORE ts]
--write ident value = call (Write ident value)

deserialize : (f : Field) -> String -> (interpField f)
deserialize (EInt _)  = prim__fromStrInt 
deserialize (EString _) = id
deserialize (EAddress _) = id

instance Handler Store IO where
  handle s (Read field)     k =
    do
      h <- openFile (show field) Read
      val <- fread h
      closeFile h
      --k (cast val) s
      k (deserialize field val) s

{-
  handle s (Write field val) {t} k =
    do
      h <- openFile (toString {t} field) Write
      fwrite h (show val)
      closeFile h
      k () s
      pureM ()
      -}











