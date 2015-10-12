
-- tag code. move label to type level
data Section : String -> Type -> Type where
  MkSection : (lbl : String) -> (x : a) -> Section lbl a
--snip
