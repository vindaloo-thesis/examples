module Bank
import Serpent
import Serpent.Prim
import Effects
import Ethereum
import Types

namespace Bank2
  deposit : {v : Nat} -> Eff ()
            [ETHEREUM (Init v)]
            [ETHEREUM (Running v 0 v)]
  deposit {v} = save v

  withdraw : (a : Nat) -> Eff Bool
             [ETHEREUM (Init 0)]
             (\success => if success
                             then [ETHEREUM (Running 0 a 0)]
                             else [ETHEREUM (Running 0 0 0)])
  withdraw a = if !sender ==
                  0x00cf7667b8dd4ece1728ef7809bc844a1356aadf
                  && !(balance !contractAddress) >= a
                  then do
                    send a !sender
                    pureM True
                  else (pureM False)

namespace Main
  runDep : Nat -> SIO ()
  runDep v = runInit [MkS v 0 0] deposit

  runWith : Nat -> SIO Bool
  runWith v = runInit [MkS 0 0 0] (withdraw v)

  main : IO ()
  main = return ()
    --res <- runInit [MkS 0 0 0] (withdraw 100)
    --putStrLn . show $ res
  --main = runInit [MkS 10 0 0] (withdraw 0)

  testList : FFI_Export FFI_Py "testHdr.py" []
  testList = Data Nat "Nat" $
             Data (List Nat) "ListNat" $
             Data (Bool) "Bool" $
             --Data (TransEff.Eff () [ETHEREUM (Init x)] [ETHEREUM (Running x 0 x)]) "EffEth" $
             --Fun deposit "deposit" $
             Fun runDep "deposit" $
             Fun Bank.Main.runWith "withdraw" $
             End
