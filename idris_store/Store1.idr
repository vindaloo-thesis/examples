module Store

import Effects
import Effect.StdIO

Ident : Type
Ident = String

Location : Type
Location = String

Value : Type
Value = String

AddressMap : Type
AddressMap = List (Ident,(Location,Type))

data Store : Effect where
  Read  : Ident -> sig Store Value AddressMap
  Write : Ident -> Value -> sig Store () AddressMap

STORE : EFFECT
STORE = MkEff AddressMap Store

instance Handler Store IO where
  handle s (Read ident)      k = case lookup ident s of
                                    Nothing  => k "" s
                                    Just loc => do
                                      h <- openFile loc Read
                                      val <- fread h
                                      closeFile h
                                      k val s
  handle s (Write ident val) k = case lookup ident s of
                                    Nothing  => k () s
                                    Just loc => do
                                      h <- openFile loc Write
                                      fwrite h val
                                      closeFile h
                                      k () s

read : Ident -> Eff Value [STORE]
read ident = call (Read ident)

write : Ident -> Value -> Eff () [STORE]
write ident value = call (Write ident value)

testRead : SimpleEff.Eff () [STORE,STDIO]
testRead = putStrLn !(read "players")

