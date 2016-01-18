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
            [STORE, ETH (Init v)]
            [STORE, ETH (Running v 0 v)]
  deposit {v} = do
    if !(read address1) == !sender
      then update balance1 (+ toIntNat v)
      else if !(read address2) == !sender
        then update balance2 (+ toIntNat v)
        else return ()
    save v
    
  withdraw : (a : Nat) -> DepEff.Eff Bool
             [STORE, ETH (Init 0)]
             (\success => if success
                             then
                              [STORE, ETH (Running 0 a 0)]
                             else [STORE, ETH (Running 0 0 0)])
  withdraw a = do
    case !sender == !(read address1) of  --case because doesn't type check with ifs. ¯\_(ツ)_/¯
         True => if !(read balance1) >= toIntNat a
                                then do
                                  update balance1 (\b => b - toIntNat a)
                                  send a !sender
                                  pureM True
                                else (pureM False)
         otherwise => case !sender == !(read address2) of
                           True => if !(read balance2) >= toIntNat a
                                                  then do
                                                    update balance2 (\b => b - toIntNat a)
                                                    send a !sender
                                                    pureM True
                                                  else (pureM False)
                           otherwise => (pureM False)

namespace Main
  runDep : Nat -> SIO ()
  runDep v = runInit [(),MkS v 0 0] deposit

  runWith : Nat -> SIO Bool
  runWith v = runInit [(),MkS 0 0 0] (withdraw v)

  main : IO ()
  main = return ()

  testList : FFI_Export FFI_Se "testHdr.se" []
  testList = Data Nat "Nat" $
             Data (Bool) "Bool" $
             Fun runDep "deposit" $
             Fun Bank.Main.runWith "withdraw" $
             End
