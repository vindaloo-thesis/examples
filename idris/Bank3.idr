module Bank

import Effects
import Ethereum
import Ethereum.Types
import Ethereum.GeneralStore

address1 : Field
address1 = EInt "address1"

address2 : Field
address2 = EInt "address2"

balance1 : Field
balance1 = EInt "balance1"

balance2 : Field
balance2 = EInt "balance2"

namespace Bank
  deposit : {v : Nat} -> TransEff.Eff ()
            [ETH (Init v), STORE]
            [ETH (Running v 0 v), STORE]
  deposit {v} = do
    if !(read address1) == !sender
      then do
        b <- read balance1
        write balance1 (b + (toIntNat v))
      else if !(read address2) == !sender
              then do
                b <- read balance2
                write balance2 (b + (toIntNat v))
              else return ()
    save v

  withdraw : (a : Nat) -> Eff Bool
             [ETH (Init 0), STORE]
             (\success => if success
                             then [ETH (Running 0 a 0), STORE]
                             else [ETH (Running 0 0 0), STORE])
  withdraw a = do
    if !(read address1) == !sender
      then if !(read balance1) >= toIntNat a
            then do
              write balance1 (b-(toIntNat a))
              send a !sender
              pureM True
            else (pureM False)
      else if !(read address2) == !sender
              then if !(read balance2) >= toIntNat a
                    then do
                      write balance2 (b-(toIntNat a))
                      send a !sender
                      pureM True
                    else (pureM False)
              else (pureM False)

namespace Main
  runDep : Nat -> SIO ()
  runDep v = runInit [MkS v 0 0,()] deposit

  runWith : Nat -> SIO Bool
  runWith v = runInit [MkS 0 0 0,()] (withdraw v)

  main : IO ()
  main = return ()
