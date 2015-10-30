module Store

import Data.HVect
import Effects
import Effect.StdIO

class Serialize t where
  deserialize : String -> t
  serialize   : t -> String

instance Serialize String where
  deserialize = id
  serialize   = id

data Map : Type -> Type -> Type where
  Empty : Map k v
  Bin   : k -> v -> Map k v -> Map k v -> Map k v

Ident : Serialize t => (t:Type) -> Type
Ident t = String

Field : Type -> Type 
Field t = String

Environment : Vect n Type -> Type
Environment ts = HVect (map Field ts)

data Store : Effect where
  Read  : Serialize t => Field t -> sig Store t (Environment ts)
--  Write : Show t => Field t -> t -> sig Store () (Environment ts)

STORE : Vect n Type -> EFFECT
STORE ts = MkEff (Environment ts) Store

--read : Serialize t => Ident t -> Eff t [STORE ts]
--read ident = call (Read ident)

--write : Show t => Ident t -> t -> Eff () [STORE ts]
--write ident value = call (Write ident value)


instance Handler Store IO where
  handle s (Read field)     k =
    do
      h <- openFile (show field) Read
      val <- fread h
      closeFile h
      k (deserialize val) s

{-
  handle s (Write field val) {t} k =
    do
      h <- openFile (toString {t} field) Write
      fwrite h (show val)
      closeFile h
      k () s
      pureM ()
      -}







