%language TypeProviders

strToType : String -> Type
strToType "Integer" = Int
strToType _ = Nat

fromFile : String -> IO (Provider Type)
fromFile fname =
  do str <- readFile fname
     return (Provide (strToType (trim str)))

%provide (T1 : Type) with fromFile "theFile"

two : T1
two = 2
