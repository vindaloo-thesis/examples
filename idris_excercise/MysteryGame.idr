module MysteryGame

import Data.Vect
import Effects
import Effect.StdIO
import Effect.Random
import Effect.System

-- guesses available, number of letters left to guess
data GState = Running Nat Nat | NotRunning

data Mystery : GState -> Type where
  Init     : Mystery NotRunning
  GameWon  : (word : String) -> Mystery NotRunning
  GameLost : (word : String) -> Mystery NotRunning
  MkG      : (word : String) ->
             (guesses : Nat) ->
             (got : List Char) ->
             (missing : Vect m Char) ->
             Mystery (Running guesses m)

data IsElem : a -> Vect n a -> Type where
  First : IsElem x (x :: xs)
  Later : IsElem x xs -> IsElem x (y ::xs)

isElem : DecEq a => (x : a) -> (xs : Vect n a) -> Maybe (IsElem x xs)
isElem _ [] = Nothing
isElem x (y :: xs) with (decEq x y)
  isElem x (x :: xs) | (Yes Refl) = Just First
  isElem x (y :: xs) | (No xneqy) with (MysteryGame.isElem x xs)
    isElem x (y :: xs) | (No xneqy) | (Just p) = Just (Later p)
    isElem x (y :: xs) | (No xneqy) | Nothing = Nothing

letters : String -> List Char
letters s = letters' $ unpack s where
  letters' [] = []
  letters' (c :: cs) = let rest = letters' cs in
                           if elem c rest then rest else c :: rest

initState : (x : String) -> Mystery (Running 6 (length (letters x)))
initState x = MkG x 6 [] (fromList $ letters x)

instance Uninhabited (IsElem x []) where
  uninhabited First impossible

shrink : (xs : Vect (S n) a) -> IsElem x xs -> Vect n a
shrink (x :: xs) First = xs
shrink (y :: []) (Later p) = absurd p -- Just for totality
shrink (y :: (x :: xs)) (Later p) = y :: shrink (x :: xs) p

data MysteryRules : Effect where
  Guess : (x : Char) ->
          sig MysteryRules Bool
          (Mystery (Running (S g) (S w)))
          (\inword => if inword
                             then Mystery (Running (S g) w)
                             else Mystery (Running g (S w)))
  Won : sig MysteryRules () (Mystery (Running g 0))
                            (Mystery NotRunning)
  Lost : sig MysteryRules () (Mystery (Running 0 w))
                             (Mystery NotRunning)
  NewWord : (w : String) ->
            sig MysteryRules () (Mystery NotRunning) (Mystery (Running 6 (length (letters w))))
  Get : sig MysteryRules String (Mystery h)

MYSTERY : GState -> EFFECT
MYSTERY h = MkEff (Mystery h) MysteryRules

instance Show (Mystery h) where
  show Init = "Game not started"
  show (GameWon w) = "Game won! Answer: " ++ w
  show (GameLost w) = "Game lost! Answer: " ++ w
  show (MkG w g got missing) = "Game running"

instance Handler MysteryRules m where
  handle (MkG w g got []) Won k = k () (GameWon w)
  handle (MkG w Z got m) Lost k = k () (GameLost w)
  handle st Get k = k (show st) st
  handle st (NewWord w) k = k () (initState w)
  handle (MkG w (S g) got m) (Guess x) k = 
    case MysteryGame.isElem x m of
         Nothing => k False (MkG w _ got m)
         (Just p) => k True (MkG w _ (x :: got) (shrink m p))

game : Eff () [MYSTERY (Running (S g) w), STDIO]
                       [MYSTERY NotRunning, STDIO]

words : ?wtype
words = with Vect ["idris","agda","haskell","miranda",
                   "java","javascript","fortran","basic",
                    "coffeescript","rust"]
wtype = proof search

runGame : Eff () [MYSTERY NotRunning, RND, SYSTEM, STDIO]
runGame = do srand !time
             let w = index !(rndFin _) words
             call $ NewWord w
             game
             putStrLn !(call Get)
