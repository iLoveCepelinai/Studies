import numpy as np
import math
#printinimo nustatymai, kad spausdintu 1 sk. po kablelio
np.set_printoptions(formatter={'float': lambda x: "{0:0.1f}".format(x)})


#sukuriame dirbtinio neurono funkcija (paduodami svoriai, iejimu reiksmes, aktyvacijos fja, bei sigmoidines fjos ribos grupems)
def neuronas(w0, w1, w2, x1, x2, aktyv, r1, r2):
    a = 1*w0 + x1*w1 + x2*w2
    if aktyv == "slen":
        if a>=0:    return 1
        else:       return 0
    else:
        f = 1/(1+math.e**(-a))
        if f <= r1: return 0  
        if f >= r2: return 1
        else:       return "gr neegz"



print("Pradedamas darbas su slenkstine aktyvacijos fja\n")
print("Svoriai, kurie tenkina salygas:\n")
#Antras punktas
#sukuriame galimu reiksmiu vektoriu (nuo -1 iki 1 kas 0.1)
gal_svor = np.round(np.arange(-1,1.1,0.1), 1)
#sukuriame lentele, kuri duota uzduotyje (naudosime ir treciame punkte)
reiks_mat = np.array([[-0.2,0.5,0],[0.2,-0.5,0],[0.8,-0.8,1],[0.8,0.8,1]])
#sukuriame atsakymu vektoriu prie kurio lipdysime tinkamus svorius
atsakymai1 = np.empty([1,3], dtype=np.uint16)

#iteruojame per nustatyta svoriu intervala
for w0 in gal_svor:
    for w1 in gal_svor:
        for w2 in gal_svor:
            #sioje vietoje mes jau turime svorius kaip kostantas
            #dabar reikia imti x is lenteles ir tikrinti visus
            #sal[0] yra x1, sal[1] yra x2, sal[3] yra neurono isejimo reiksme
            counter = 0
            #imame viena lenteles eilute (viena salyga)
            for sal in reiks_mat:
                #skaiciuojame suma
                y = neuronas(w0,w1,w2,sal[0],sal[1],"slen",0,0)#gale 0, nes sigmoidine mums dabar nesvarbu
                #kreipiames i aktyvacijos fja, jei salygoje duota klase nesutampa su isejimu, nustojame tikrinti svoriu kombinacija ir einame prie kitos
                if y != sal[2]:
                    break
                #counter padeda sekti kiek salygu buvo patikrintos
                counter += 1
            #jei visos 4 salygos patvirtintos, musu svoriai tinkami pagal salyga ir yra itraukiami i galimu atsakymu masyva
            if counter == 4:
                print("w0 = ", w0, ", w1 = ", w1, ", w2 = ", w2)
                atsakymai1 = np.append(atsakymai1, [[w0,w1,w2]], axis = 0)


atsakymai1 = np.delete(atsakymai1, 0, axis = 0)
#print(atsakymai1)

#Trecias punktas (sigmoidine funkcija)
#Padidiname svoriu intervala, nes su sigmoidine atsimes daugiau reiksmiu
print("\n Pradedamas darbas su sigmoidine aktyvacijos fja\n")
print("Svoriai, kurie tenkina salygas:\n")
gal_svor2 = np.round(np.arange(-5,5.1,0.1), 1)
atsakymai2 = np.empty([1,3], dtype=np.uint16)

#sukurkime norimas ribas musu sigmoidinei funkcijai atrinkti. Paimkime iki 0.1 (imtinai) 0ai klasei ir nuo 0.9 1ai klasei
r1 = 0.2
r2 = 0.8

for w0 in gal_svor2:
    for w1 in gal_svor2:
        for w2 in gal_svor2:
            counter = 0
            #imame viena lenteles eilute (viena salyga)
            for sal in reiks_mat:
                #kreipiamasi i neurona, kad gautu isejima
                y = neuronas(w0,w1,w2,sal[0],sal[1],"sig",r1,r2)
                if y != sal[2]:
                    break
                #counter padeda sekti kiek salygu buvo patikrintos
                counter += 1
            #jei visos 4 salygos patvirtintos, musu svoriai tinkami pagal salyga ir yra itraukiami i galimu atsakymu masyva
            if counter == 4:
                print("w0 = ", w0, ", w1 = ", w1, ", w2 = ", w2)
                atsakymai2 = np.append(atsakymai2, [[w0,w1,w2]], axis = 0)

atsakymai2 = np.delete(atsakymai2, 0, axis = 0)
#print(atsakymai2)