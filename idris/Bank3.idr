module Bank

import Effects
import Ethereum
import Ethereum.SIO
import Ethereum.Types
import Ethereum.Environment
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
  -- TODO: ether leak
  deposit : {v : Nat} -> TransEff.Eff ()
            [STORE, ETH_IN v b, ENV c s o]
            [STORE, ETH_OUT v b 0 v, ENV c s o]
  deposit {v} {s} = do
    if !(read address1) == s
      then update balance1 (+ toIntNat v)
      else if !(read address2) == s
        then update balance2 (+ toIntNat v)
        else return ()
    save v
    
  withdraw : (a : Nat) -> DepEff.Eff Bool
             [STORE, ETH_IN 0 b, ENV c s o]
             (\success => if success
                             then [STORE, ETH_OUT 0 b a 0, ENV c s o]
                             else [STORE, ETH_OUT 0 b 0 0, ENV c s o])
  withdraw a {s} = do
    case s == !(read address1) of  --case because doesn't type check with ifs. ¯\_(ツ)_/¯
         True => if !(read balance1) >= toIntNat a
                                then do
                                  update balance1 (\b => b - toIntNat a)
                                  send a s
                                  pureM True
                                else (pureM False)
         otherwise => case s == !(read address2) of
                           True => if !(read balance2) >= toIntNat a
                                                  then do
                                                    update balance2 (\b => b - toIntNat a)
                                                    send a s
                                                    pureM True
                                                  else (pureM False)
                           otherwise => (pureM False)

namespace Main
  runDep : SIO ()
  runDep = runInit [(),MkS prim__value prim__balance 0 0, MkE 0x1 0x2 0x2] deposit

  runWith : Nat -> SIO Bool
  runWith a = runInit [(),MkS 0 prim__balance 0 0, MkE 0x1 0x2 0x2] (withdraw a)

  main : IO ()
  main = return ()

  testList : FFI_Export FFI_Se "testHdr.se" []
  testList = Data Nat "Nat" $
             Data (Bool) "Bool" $
             Fun runDep "deposit" $
             Fun Bank.Main.runWith "withdraw" $
             End
