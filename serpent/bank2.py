import serpent
from ethereum import tester, utils, abi
serpent_code = '''
def idris_Effects_46_dropEnv(v0, v1, v2, v3, v4): #Effects.dropEnv
    if v4[0] == 2:
      if v3[0] == 1:
        return self.idris_Effects_46_dropEnv(0, 0, 0, v3[3], v4[1])
    elif v4[0] == 1:
      if v3[0] == 1:
        retVal = [1,v6,v3[2],self.idris_Effects_46_dropEnv(0, 0, 0, v3[3], v4[1])]
        return retVal
    elif v4[0] == 0:
      return [0]

def idris_Effects_46_eff(v0, v1, v2, v3, v4, v5, v6, v7): #Effects.eff
    if v6[0] == 5:
      return self.idris_Effects_46_eff(0, 0, 0, 0, 0, [1,v5[1],v5[2],[0]], v6[1], [65693,v7])
    elif v6[0] == 2:
      return self.idris_Effects_46_execEff(0, 0, 0, 0, 0, 0, 0, v5, v6[1], v6[2], v7)
    elif v6[0] == 1:
      return self.idris_Effects_46_eff(0, 0, 0, 0, 0, v5, v6[1], [65695,v6[2],v7])
    elif v6[0] == 3:
      v15 = self.idris_Effects_46_dropEnv(0, 0, 0, v5, v6[1])
      v16 = [65697,v7,v6[1],v5]
      retVal = self.idris_Effects_46_eff(0, 0, 0, 0, 0, v15, v6[2], v16)
      return retVal
    elif v6[0] == 4:
      return self.idris_Effects_46_eff(0, 0, 0, 0, 0, [1,v6[1],v6[2],v5], v6[3], [65698,v7])
    elif v6[0] == 0:
      return self.idris__123_APPLY0_125_(self.idris__123_APPLY0_125_(v7, v6[1]), v5)

def idris_Effects_46_execEff(v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10): #Effects.execEff
    if v8[0] == 0:
      if v7[0] == 1:
        v14 = self.idris_Effects_46_handle(0, 0, 0, 0, 0, 0, v7[1])
        v14 = self.idris__123_APPLY0_125_(v14, v7[2])
        v14 = self.idris__123_APPLY0_125_(v14, v9)
        v15 = [65700,v10,v7[1],v7[3]]
        return self.idris__123_APPLY0_125_(v14, v15)
    elif v8[0] == 1:
      if v7[0] == 1:
        return self.idris_Effects_46_execEff(0, 0, 0, 0, 0, 0, 0, v7[3], v8[1], v9, [65702,v10,v7[1],v7[2]])
def idris_Effects_46_handle(v0, v1, v2, v3, v4, v5, v6): #Effects.handle
    v7 = self.idris__123_APPLY0_125_(v6, v2)
    v7 = self.idris__123_APPLY0_125_(v7, v3)
    v7 = self.idris__123_APPLY0_125_(v7, v4)
    return self.idris__123_APPLY0_125_(v7, v5)

def idris_Effects_46_rebuildEnv(v0, v1, v2, v3, v4, v5, v6): #Effects.rebuildEnv
    if v5[0] == 2:
      if v6[0] == 1:
        v11 = self.idris_Effects_46_rebuildEnv(0, 0, 0, 0, v4,  v5[1], v6[3])
        retVal = [1,v6[1],v6[2],v11]
        return retVal
    elif v5[0] == 1:
      v7 = v5[1]
      if v4[0] == 1:
        v8 = v4[1]
        v9 = v4[2]
        v10 = v4[3]
        if v6[0] == 1:
          v14 = self.idris_Effects_46_rebuildEnv(0, 0, 0, 0, v10, v7, v6[3])
          retVal = [1,v8,v9,v14]
          return retVal
      elif v4[0] == 0:
        return [0]

    elif v5[0] == 0:
      if v4[0] == 1:
        retVal = [1,v4[1],v4[2],v4[3]]
        return retVal
      elif v4[0] == 0:
        return v6


def idris_Effects_46_relabel(v0, v1, v2, v3, v4): #Effects.relabel
    if v4[0] == 1:
      retVal = [1,v4[1],v4[2],self.idris_Effects_46_relabel(0, 0, 0, 0, v4[3])]
      return retVal
    elif v4[0] == 0:
      return [0]

def idris_Bank_46_Main_46_runDep(v0): #Bank.Main.runDep
    return self.idris_Effects_46_eff(0, 0, 0, 0, 0, [1,[65677],v0,[0]], [2,[0],[4]], [65704,[65670]])
def idris_Bank_46_Main_46_runWith(v0): #Bank.Main.runWith
    return self.idris_Effects_46_eff(0, 0, 0, 0, 0,  [1,[65686],0,[0]], [1,[2,[0],[3]],[65668,0,v0]], [65704,[65679]])
def idris__123_APPLY0_125_(v0, v1): #{APPLY0}
    if v0[0] == 65664:
      retVal = [0,[1]]
      return retVal
    elif v0[0] == 65665:
      retVal = [1, [2,[0],[5,v0[1],v1]], [65664]]
      return retVal
    elif v0[0] == 65666:
      return self.idris_Bank_46_Bank2_46__123_withdraw3_125_(v0[1], v0[2], v0[3], v1)
    elif v0[0] == 65667:
      retVal = [1,[2,[0],[2,v1]],[65666,v0[2],v0[3],v0[1]]]
      return retVal
    elif v0[0] == 65668:
      retVal = [1,[2,[0],[0]],[65667,v0[1],v1,v0[2]]]
      return retVal
    elif v0[0] == 65669:
      retVal = [65715,0,0,v1]
      return retVal
    elif v0[0] == 65670:
      return [65669]
    elif v0[0] == 65671:
      return self.idris_Effects_46_Ethereum_46_Ether_46__64_Effects_46_Handler_36_EtherRules_58_SIO_58__33_handle_58_0(0, 0, 0, 0, v0[1], v0[2], v1)
    elif v0[0] == 65672:
      retVal = [65671,v0[1],v1]
      return retVal
    elif v0[0] == 65673:
      retVal = [65672,v1]
      return retVal
    elif v0[0] == 65674:
      return [65673]
    elif v0[0] == 65675:
      return [65674]
    elif v0[0] == 65676:
      return [65675]
    elif v0[0] == 65677:
      return [65676]
    elif v0[0] == 65678:
      retVal = [65715,0,0,v1]
      return retVal
    elif v0[0] == 65679:
      return [65678]
    elif v0[0] == 65680:
      return self.idris_Effects_46_Ethereum_46_Ether_46__64_Effects_46_Handler_36_EtherRules_58_SIO_58__33_handle_58_0(0, 0, 0, 0, v0[1], v0[2], v1)
    elif v0[0] == 65681:
      retVal = [65680,v0[1],v1]
      return retVal
    elif v0[0] == 65682:
      retVal = [65681,v1]
      return retVal
    elif v0[0] == 65683:
      return [65682]
    elif v0[0] == 65684:
      return [65683]
    elif v0[0] == 65685:
      return [65684]
    elif v0[0] == 65686:
      return [65685]
    elif v0[0] == 65687:
      return self.idris__123_APPLY0_125_(self.idris__123_APPLY0_125_(v0[10], v0[13]), v1[3])
    elif v0[0] == 65688:
      return self.idris__123_APPLY0_125_(self.idris__123_APPLY0_125_(v0[1], v1), v0[2])
    elif v0[0] == 65689:
      return self.idris__123_APPLY0_125_(self.idris__123_APPLY0_125_(v0[1], v1), v0[2])
    elif v0[0] == 65690:
      return self.idris__123_APPLY0_125_(self.idris__123_APPLY0_125_(v0[1], v0[2]))
    elif v0[0] == 65691:
      v0 = self.idris__123_APPLY0_125_(v0[1], v1)
      return self.idris__123_APPLY0_125_(v0, v0[2])
    elif v0[0] == 65692:
      return self.idris__123_APPLY0_125_(self.idris__123_APPLY0_125_(v0[1], v0[2]), self.idris_Effects_46_relabel(0, 0, 0, 0, v1))
    elif v0[0] == 65693:
      retVal = [65692,v0,v1]
      return retVal
    elif v0[0] == 65694:
      return self.idris_Effects_46_eff(0, 0, 0, 0, 0, v1, self.idris__123_APPLY0_125_(v0[1], v0[2]), v0[3])
    elif v0[0] == 65695:
      retVal = [65694,v0[1],v1,v0[2]]
      return retVal
    elif v0[0] == 65696:
      return self.idris__123_APPLY0_125_(self.idris__123_APPLY0_125_(v0[1], v0[2]), self.idris_Effects_46_rebuildEnv(0, 0, 0, 0, v1, v0[3], v0[4]))
    elif v0[0] == 65697:
      retVal = [65696,v0[1],v1,v0[2],v0[3]]
      return retVal
    elif v0[0] == 65698:
      retVal = [65687,0,0,0,0,0,0,0,0,0,v0[1],0,0,v1,0]
      return retVal
    elif v0[0] == 65699:
      return self.idris__123_APPLY0_125_(self.idris__123_APPLY0_125_(v0[1], v0[2]), [1,v0[3],v1,v0[4]])
    elif v0[0] == 65700:
      retVal = [65699,v0[1],v1,v0[2],v0[3]]
      return retVal
    elif v0[0] == 65701:
      return self.idris__123_APPLY0_125_(self.idris__123_APPLY0_125_(v2, v3), [1,v4,v5,v1])
    elif v0[0] == 65702:
      retVal = [65701,v0[1],v1,v0[2],v0[3]]
      return retVal
    elif v0[0] == 65703:
      return self.idris__123_APPLY0_125_(self.idris__123_APPLY0_125_(v0[1], 0), v0[2])
    elif v0[0] == 65704:
      retVal = [65703,v0[1],v1]
      return retVal
    elif v0[0] == 65705:
      v2 = v0[1]
      return v2.balance 
    elif v0[0] == 65706:
      return self
    elif v0[0] == 65707:
      return send(v0[1], v0[2]) #self.idris_Ethereum_46_SIO_46_send(v2, v3, v1)
    elif v0[0] == 65708:
      return msg.sender #self.idris_Ethereum_46_SIO_46_sender(v1)
    elif v0[0] == 65709:
      return self.idris_Prelude_46_Classes_46_Prelude_46_Nat_46__64_Prelude_46_Classes_46_Eq_36_Nat_58__33__61__61__58_0(v0[1], v1)
    elif v0[0] == 65710:
      return self.idris_Prelude_46_Classes_46_Prelude_46_Nat_46__64_Prelude_46_Classes_46_Ord_36_Nat_58__33_compare_58_0(v0, v1)
    elif v0[0] == 65711:
      retVal = [65710,v1]
      return retVal
    elif v0[0] == 65712:
      return self.idris_Prelude_46_Nat_46__123_Nat_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam2_125_(v0[1], v1)
    elif v0[0] == 65713:
      retVal = [65712,v1]
      return retVal
    elif v0[0] == 65714:
      return self.idris__123_APPLY0_125_([65716,v0[1],v0[2],v0[3],v0[4],v0[5],v1], self.idris__123_APPLY0_125_(v0[4], v1))
    elif v0[0] == 65715:
      return v4 #self.idris_io_95_return(v2, v3, v4, v1)
    elif v0[0] == 65716:
      return self.idris__123_APPLY0_125_(self.idris__123_APPLY0_125_(v0[5], v1), v0[6])
    elif v0[0] == 65717:
      retVal = [65709,v1]
      return retVal
    else:
      return 0

def idris_Prelude_46_Nat_46__123_Nat_32_instance_32_of_32_Prelude_46_Classes_46_Ord_95_lam2_125_(v0, v1): #Prelude.Nat.{Nat instance of Prelude.Classes.Ord_lam2}
    #legonote compare fishy
    v2 = self.idris__123_APPLY0_125_(self.idris__123_APPLY0_125_([65711], v0), v1)
    if v2[0] == 2:
      return [1]
    else:
      return [0]

def idris_Bank_46_Bank2_46__123_withdraw3_125_(v0, v1, v2, v3): #Bank.Bank2.{withdraw3}
    if v0 == 701581095649848031: #legonote: or opposite? maybe == should be 0
      v4 = self.idris_Prelude_46_Classes_46_Prelude_46_Nat_46__64_Prelude_46_Classes_46_Ord_36_Nat_58__33__62__61__58_0(v3, v1)
    else:
      v4 = [0]
    if v4[0] == 0:
      retVal = [0,[0]]
      return retVal
    elif v4[0] == 1:
      retVal = [1,[2,[0],[3]],[65665,v1,v2]]
      return retVal

def idris_Prelude_46_Classes_46_Prelude_46_Nat_46__64_Prelude_46_Classes_46_Eq_36_Nat_58__33__61__61__58_0(v0, v1): #Prelude.Classes.Prelude.Nat.@Prelude.Classes.Eq$Nat:!==:0
    if v1 == 0:
      if v0 == 0:
        return [1]
      else:
        return [0]
    else:
      if v0 == 0:
        return [0]
      return self.idris__123_APPLY0_125_(self.idris__123_APPLY0_125_([65717], v0-1), v1-1)

def idris_Effects_46_Ethereum_46_Ether_46__64_Effects_46_Handler_36_EtherRules_58_SIO_58__33_handle_58_0(v0, v1, v2, v3, v4, v5, v6): #Effects.Ethereum.Ether.@Effects.Handler$EtherRules:SIO:!handle:0
    if v5[0] == 2:
      retVal = [65714,0,0,0,[65705,v5[1]],[65688,v6,v4]]
      return retVal
    elif v5[0] == 0:
      retVal = [65714,0,0,0,[65706],[65689,v6,v4]]
      return retVal
    elif v5[0] == 4:
      return self.idris__123_APPLY0_125_(self.idris__123_APPLY0_125_(v6, [0]), v4)
    elif v5[0] == 5:
      retVal = [65714,0,0,0,[65707,v5[2],v5[1]],[65690,v6,v4]]
      return retVal
    elif v5[0] == 3:
      retVal = [65714,0,0,0,[65708],[65691,v6,v4]]
      return retVal
    elif v5[0] == 1:
      return self.idris__123_APPLY0_125_(self.idris__123_APPLY0_125_(v6, v4), v4)

def idris_Prelude_46_Classes_46_Prelude_46_Nat_46__64_Prelude_46_Classes_46_Ord_36_Nat_58__33__62__61__58_0(v0, v1): #Prelude.Classes.Prelude.Nat.@Prelude.Classes.Ord$Nat:!>=:0
    v2 = self.idris__123_APPLY0_125_([65713], v0) #legonote ordshit
    v2 = self.idris__123_APPLY0_125_(v2, v1)
    if v2[0] == 0:
      return self.idris_Prelude_46_Classes_46_Prelude_46_Nat_46__64_Prelude_46_Classes_46_Eq_36_Nat_58__33__61__61__58_0(v0, v1)
    elif v2[0] == 1:
      return  [1]

def idris_Prelude_46_Classes_46_Prelude_46_Nat_46__64_Prelude_46_Classes_46_Ord_36_Nat_58__33_compare_58_0(v0, v1): #Prelude.Classes.Prelude.Nat.@Prelude.Classes.Ord$Nat:!compare:0
    if v1 == 0:
      if v0 == 0:
        return [1]
      #legonote wtf @ v2, not used
      return [2]
      #v2 = 1
      #v2 = (v0 - v2)
      #retVal = [2]
      #return retVal
    v2 = (v1 - 1)
    if v0 == 0:
      return [0]
    return self.idris__123_APPLY0_125_(self.idris__123_APPLY0_125_([65711], v0 - 1), v2)

'''



#Generate public keys
public_k0 = utils.privtoaddr(tester.k0)
public_k1 = utils.privtoaddr(tester.k1)
#Generate state and add contract to bvk chain
s = tester.state()
print("Tester state created")
c = s.abi_contract(serpent_code)
print("Code added to bvk chain")
print(c)
#Test Contract
o = c.idris_Bank_46_Main_46_runDep(10)
print "Result of dep 10: " + str(o)
o = c.testme()
print "Result of testme: " + str(o)
o = c.idris_Bank_46_Main_46_runWith(10)
print "Result of with 10: " + str(o)
'''
o = c.send_currency_to(1000, public_k1)
if o == 1:
	print("$1000 sent to tester_k1 from tester_k0")
else:
	print("Failed to send $1000 to tester_k1 from tester_k0")
o = c.send_currency_to(10000, public_k1)
if o == 1:
	print("$10000 sent to tester_k1 from tester_k0")
else:
	print("Failed to send $10000 to tester_k1 from tester_k0")
o = c.balance_check(public_k0)
print("tester_k0 has a balance of " + str(o))
o = c.balance_check(public_k1)
print("tester_k1 has a balance of " + str(o))
'''
