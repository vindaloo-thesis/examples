module Tests
( fun1
, playerInput  
) where

import Control.Applicative
import Control.Monad.State

data Ethereum s a
data Address

instance Functor (Ethereum s) where
  fmap = undefined

instance Applicative (Ethereum s) where
  (<*>) = undefined
  pure  = undefined

instance Monad (Ethereum s) where
  (>>=)  = undefined
  return = undefined 

balance :: Ethereum s Int
balance = undefined

sender :: Ethereum s Address
sender = undefined

send :: Int -> Address -> Ethereum s ()
send = undefined

givenStatePart :: MonadState s m => (s -> a) -> (a -> Bool) -> m () -> m ()
givenStatePart f p action = flip when action =<< liftM p (gets f)

givenState :: MonadState s m => (s -> Bool) -> m () -> m ()
givenState = givenStatePart id 

given :: Monad m => m a -> (a -> Bool) -> m () -> m ()
given ma p action = flip when action =<< liftM p ma

(<:>) :: Monad m => (a -> b -> m c) -> m a -> m (b -> c)
f <:> ma = return $ join . \b -> do
    a <- ma
    f a b


{-

register key val = givenState (not . member key) $ modify (insert key val)


----------

fun1 :: Ethereum s ()
fun1 = do
    given balance (/=0) $ do
      send <:> balance <*> sender
      ja,hfkh
      kjsghk
    given 

send :: Int -> Address -> Ethereum s ()
balance :: Ethereum s Int
sender :: Ethereum s Address

send =<< balance :: 

-}

data Commitment a

data Choice = Rock | Paper | Scissors
  deriving (Eq,Show,Read)

data GameState = Zero
               | One { reward :: Int
                     , p1     :: (Address,Commitment Choice)
                     }
               | Two { reward :: Int
                     , p1     :: (Address,Commitment Choice)
                     , p2     :: (Address,Commitment Choice)
                     }



playerInput :: Choice -> Ethereum GameState ()
playerInput c = undefined -- givenState (==Zero) $ put $ One <$> value <*> sender <*> pure m 

nextState :: GameState -> Address -> Commitment Choice -> Int -> GameState
nextState Zero a c v = One { reward = v, p1 = (a,c) }
