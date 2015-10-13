module RPS2

data Address = Addr Int
data Commit a = Comm a
data Choice = Rock | Paper | Scissors

record Game where
  constructor MkGame
  player1 : Maybe (Address,Commit Choice)
  player2 : Maybe (Address,Commit Choice)

data Finalizable : Game -> Type where
  TwoPlayers : Finalizable (MkGame (Just p1) (Just p2))

isFinalizable : (g : Game) -> Dec (Finalizable g)
isFinalizable (MkGame (Just p1) (Just p2)) = Yes TwoPlayers
isFinalizable (MkGame (Just p1) Nothing)   = No (\TwoPlayers impossible)
isFinalizable (MkGame Nothing   (Just p2)) = No (\TwoPlayers impossible)
isFinalizable (MkGame Nothing   Nothing)   = No (\TwoPlayers impossible)

{-
finalize : { [ETHEREUM] } Eff ()
finalize = case isFinalizable !state of
            Just p  => sendMoney !state p
            Nothing => return ()

sendMoney : (g : Game) -> Finalizable g -> { [ETHEREUM] } Eff ()
sendMoney (MkGame (Just (p1,c1)) (Just (p2,c2))) Yes =
    case winner !(open c1) !(open c2) of
      First  => send !balance p1
      Second => send !balance p2
      Tie    => let payout = !balance/2
                 in send payout p1 >> send payout p2
-}