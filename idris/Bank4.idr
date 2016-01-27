module Bank

import Effects
import Ethereum
import Ethereum.SIO

%default total

address1 : Field
address1 = EInt "address1"

address2 : Field
address2 = EInt "address2"

balance1 : Field
balance1 = EInt "balance1"

balance2 : Field
balance2 = EInt "balance2"

namespace Bank
  deposit : {v : Nat} -> DepEff.Eff Bool
            [STORE, ETH v b 0 0, ENV c s o]
            (\success => if success
                           then [STORE, ETH v b 0 v, ENV c s o]
                           else [STORE, ETH v b v 0, ENV c s o])
  deposit {v} {s} = if !(read address1) == s
      then do
        update balance1 (+ toIntNat v)
        save v
        pureM True
      else if !(read address2) == s
        then do
          update balance2 (+ toIntNat v)
          save v
          pureM True
        else do
          send v s
          pureM False
    
  withdraw : (a : Nat) ->
             DepEff.Eff Bool
             [STORE, ETH 0 b 0 0, ENV c s o]
             (\success => if success
                             then [STORE, ETH 0 b a 0, ENV c s o]
                             else [STORE, ETH 0 b 0 0, ENV c s o])
  withdraw a {s} = do
    case s == !(read address1) of  --case because doesn't type check with ifs. ¯\_(ツ)_/¯
         True => if !(read balance1) >= toIntNat a
                                then do
                                  update balance1 (\b => b - toIntNat a)
                                  send a s
                                  pureM True
                                else (pureM False)
         _    => case s == !(read address2) of
                           True => if !(read balance2) >= toIntNat a
                                                  then do
                                                    update balance2 (\b => b - toIntNat a)
                                                    send a s
                                                    pureM True
                                                  else (pureM False)
                           otherwise => (pureM False)

namespace Main
  runDep : SIO Bool
  runDep = runInit [(),MkS prim__value prim__selfbalance 0 0, MkE 0x1 0x2 0x2] deposit

  runWith : Nat -> SIO Bool
  runWith a = case prim__value == 0 of
                   True => do
                     runInit [(), MkS 0 prim__selfbalance 0 0, MkE 0 0x00cf7667b8dd4ece1728ef7809bc844a1356aadf 0] (withdraw a)
                     return True
                   False => return False

  main : IO ()
  main = return ()

  testList : FFI_Export FFI_Se "testHdr.se" []
  testList = Data Nat "Nat" $
             Data (Bool) "Bool" $
             Fun runDep "deposit" $
             Fun Bank.Main.runWith "withdraw" $
             End
