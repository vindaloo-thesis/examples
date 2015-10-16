module Ethereum

import Effects
import Effect.Exception

data Address = Addr Int
data Commit a = Comm a


data Ethereum : Effect where
--  Get :      sig State a  a
--  Put : b -> sig State () a b

ETHEREUM : EFFECT
ETHEREUM = MkEff () Ethereum

send : Nat -> Address -> Eff () [ETHEREUM]
send amount to = return ()

balance : Eff Nat [ETHEREUM]
balance = return 0

value : Eff Nat [ETHEREUM]
value = return 0

sender : Eff Address [ETHEREUM]
sender = return (Addr 0)

open : Commit a -> Eff a [ETHEREUM]
open (Comm a) = return a

require : Bool -> a -> Eff a [EXCEPTION String,ETHEREUM]
require True  a = return a
require False a = do
  if !value <= !balance
    then send !value !sender
    else send !balance !sender
  raise "Invalid call, all ether returned."