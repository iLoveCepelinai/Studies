#pandas reikalinga duomenu tvarkybai
import pandas as pd
#numpy geresniems masyvams
import numpy as np
#reikes funkcijai floor
import math
#su os keiciu darbo direktorija (visual studio issidirbineja)
import os
import matplotlib.pyplot as plt

#os.getcwd()
#os.chdir('D:\\Users\\User\\Desktop\\Studijos\\trecias kursas\\DI pagrindai\\DNT\\Uzduotys\\Uzduotis2\\Uzduotis2')

#########################################################################################################
#Duomenu tvarkymas
def duomenu_tvarkyba(duom_pav, duom_pav1, duom_pav2):
    #nuskaitome iris.data duomenis
    duom = pd.read_csv(duom_pav, header = None, sep = ',')
    #pakeisciame stulpeliu vardus i lietuviskus
    duom.columns = ['taurel_ilg','taurel_plot','ziedl_ilg','ziedl_plot','klase']


    #kusime 2 failus, viena, kur Setosa bus klaseje 0 ir kitos - 1, antra, kur Versicolor bus 0 klaseje, o virginice - 1.
    #pirma sukuriame nauja lentele kuri yra originalios kopija
    duom1 = duom.copy()
    #pakeiciame setosa i 0, o kitas - i 1
    duom1["klase"].replace({"Iris-setosa": 0, "Iris-virginica": 1, "Iris-versicolor":1}, inplace = True)


    #Sukuriame antra kopija originalios lenteles naujoms klasems, kur Versicolor bus 0 klaseje, o Virginca - 1
    duom2 = duom.copy()
    #Pries tai pasaliname nereikalinga Setosa klase
    duom2.drop(duom2[duom2.klase == "Iris-setosa"].index, inplace=True)
    duom2["klase"] = duom2["klase"].replace({"Iris-virginica": 1, "Iris-versicolor":0})


    #Taigi turime 2 lenteles. Irasykime i faila, kad turetume ateiciai:
    duom1.to_csv(duom_pav1, index=False)
    duom2.to_csv(duom_pav2, index=False)

#Dirbtinis neuronas.
def neuronas(svoriai, x1, x2, x3, x4, aktyv):
    a = 1*svoriai[0] + x1*svoriai[1] + x2*svoriai[2] + x3*svoriai[3] + x4*svoriai[4]
    if aktyv == "slen":
        if a>=0:    return 1
        else:       return 0
    else:
        f = 1/(1+math.e**(-a))
        return f
    
#Dirbtinio neurono mokymas
def mokymas(mokDuom, aktyv, iterac_sk, epochu_sk, mok_greit, spausdint = False, grafikas = False):
    np.random.seed(419)#nustatome, kad atsitiktinumas butu vienodas visoms versijoms
    svoriai = np.random.uniform(size=5)#sugeneruojame 5 svorius pradziai

    iter_temp = len(mokDuom)*epochu_sk # tikrinsime, su kuria riba bus mokomasi ilgiau
    if iterac_sk < iter_temp: iterac_sk = iter_temp # jei epochose bus daugiau iteraciju nei duota, tai mokysim tie, kiek yra epochu

    epochu_sk = math.floor(iterac_sk/len(mokDuom))#gauname kiek minimum bus epochu
    iter_liek = iterac_sk % len(mokDuom) #gauname kiek dar reiks atlikti iteraciju praejus visas epochas

    paklaida = 0
    klas_tiksl = 0
    ep_nr = 1

    paklaidos = np.empty([1,2])
    paklaidos = np.delete(paklaidos, 0, axis = 0)
    #ciklas, skirtas eiti tol, kol pasiekeme uzsibrezta epochu skaiciu
    while ep_nr <= epochu_sk:
        #mokymosi klasifikavimo tikslumas
        klas_tiksl = 0
        #mokymosi paklaida
        paklaida = 0
        for iterac in range(len(mokDuom)):
            iejimai = mokDuom.iloc[iterac]#nuskaitome lenteles eilute
            y = neuronas(svoriai, iejimai[0], iejimai[1], iejimai[2], iejimai[3], aktyv)
            #naudojame round jei sigmoidine funkcija, tai is intervalo (0,1) taps klase 0 arba 1
            if round(y,0) != iejimai[4]:
                svoriai[0]=svoriai[0]+mok_greit*(iejimai[4]-y)*1
                for i in range(4):
                    svoriai[i+1]=svoriai[i+1] + mok_greit * (iejimai[4]-y)*iejimai[i]
            else: klas_tiksl += 1
            paklaida += (iejimai[4] - y)**2
        paklaidos = np.append(paklaidos, [[ep_nr, 0.5*paklaida]], axis = 0)
        ep_nr+=1


    #ciklas, skirtas eiti tol, kol pasiekeme norima iteraciju skaiciu (tuo atveju, jei norimu iteraciju sk. nesidalina
    #is duomenu rinkinio dydzio)
    for iterac in range(iter_liek):
        iejimai = mokDuom.iloc[iterac]#sukuriame iejimu vektoriu + klase (nuskaitome eilute lenteles)
        y = neuronas(svoriai, iejimai[0], iejimai[1], iejimai[2], iejimai[3], aktyv)
        if round(y,0) != iejimai[4]:
            svoriai[0] = svoriai[0] + mok_greit *(iejimai[4]-y)
            for i in range(4):
                svoriai[i+1]=svoriai[i+1]+mok_greit*(iejimai[4]-y)*iejimai[i]

    #tam, kad paklaidu grazinimas veiktu su iteracijomis, su kuriuomis nesigauna pilna epocha
    if(iter_liek == 0): gal_pakl = paklaidos[-1,1]
    else: 
        #kazkoks didelis skaicius, kad mums aiskiai parodyti, kad nekorektiska lyginti
        gal_pakl = 1000000
        klas_tiksl = 0

    klas_tiksl = klas_tiksl/len(mokDuom)
    svoriai = np.append(svoriai, [[klas_tiksl, gal_pakl]])
    if grafikas:
        pakl_graf(paklaidos, aktyv)

    if spausdint:
        mok_info(svoriai)

    return svoriai

#Pagalbine mokymo funkcija, jei norime spausdinti gautas reiksmes (svorius, paklaida, rusiavimo tiksluma)
def pakl_graf(paklaidos, aktyv):
    plt.plot(paklaidos[:,0], paklaidos[:,1])
    if aktyv == 'slen':
        plt.title('Slenkstinės funkcijos paklaida nuo epochų skaičiaus')
    else: plt.title('Sigmoidinės funkcijos paklaida nuo epochų skaičiaus')
    plt.xlabel('Epochų skaičius')
    plt.ylabel('Paklaida')
    plt.show()

#Pagalbine mokymo funkcija, jei norime spausdinti paklaidos grafika
def mok_info(svoriai):
    print("Gauti svoriai: w_0 = {0}, w_1 = {1}, w_2 = {2}, w_3 = {3}, w_4 = {4}\n".format(svoriai[0], 
          svoriai[1], svoriai[2], svoriai[3], svoriai[4]))
    print("Klasifikavimo tikslumas (mokymo) = ", svoriai[5], "\n")
    print("Paklaida = ", svoriai[6])

#Dirbtinio neurono testavimas
def testavimas(svoriai, testDuom, aktyv):
    #ivedame klasifikavimo tiksluma
    klas_tiksl = 0
    #einame per testDuom lentele (20 % visu duomenu)
    for iter in range(len(testDuom)):
        #nuskaitome salyga (eilute is lenteles)
        salyga = testDuom.iloc[iter]
        #liepiame neuronui nustatyti klase su konkreciais svoriais
        y=neuronas(svoriai, salyga[0], salyga[1], salyga[2], salyga[3], aktyv)
        #tikriname, ar neuronas parinko teisinga klase (apvaliname del sigmoidines funkcijos)
        if salyga[4] == round(y, 0):
            #jei teisinga, sumuojame visas teisingai parinktas klases
            klas_tiksl+=1
    #uzbaigiame klasifikavimo tikslumo formule
    klas_tiksl = klas_tiksl / len(testDuom)
    #graziname paklaida ir klasivfikavimo tiksluma
    return klas_tiksl

#Paprastas mokymas + testavimas (spausdina svorius ir testiniu duomenu klasifikavimo tiksluma)
def mokTest(mokDuom, testDuom, aktyv, iter_sk, epochu_sk, mok_greit):
    #gauname neurono svorius
    svoriai = mokymas(mokDuom, aktyv, iter_sk, epochu_sk, mok_greit)
    #einame per testDuom lentele (20 % visu duomenu)
    print(" i\ty_i\tt_i")
    for iter in range(len(testDuom)):
        #nuskaitome salyga (eilute is lenteles)
        salyga = testDuom.iloc[iter]
        #liepiame neuronui nustatyti klase su konkreciais svoriais
        y=neuronas(svoriai, salyga[0], salyga[1], salyga[2], salyga[3], aktyv)
        #lyginame klases. Apvalinu iki 5, nes sigmoidine funkcija gali tureti labai maza skirtuma nuo 0 ar 1
        #print(iter+1,"%7d"%",round(y,5),"\t\t",salyga[4])
        print('{: >2}  {: >7}\t{:0}'.format(iter+1, round(y,5), salyga[4]))
    return

#Testavimas pagal iteraciju skaiciu (tyrimui)
def test_iter(mokDuom, testDuom, aktyv, iter_sk, mok_greit):
    for greitis in mok_greit:
        greitis = round(greitis, 1)
        #greitis = round(greitis, 2)
        atsakymai = np.empty([1,3])
        atsakymai = np.delete(atsakymai, 0, axis = 0)
        for iter in iter_sk:
            svoriai = mokymas(mokDuom, aktyv, iter, 0, greitis)
            atsak = testavimas(svoriai, testDuom, aktyv)
            atsakymai = np.append(atsakymai, [[atsak, greitis, iter]], axis = 0)
        plt.plot(atsakymai[:,2], atsakymai[:,0], label = str(greitis))
    
    plt.legend(title = 'Greičiai')

    if aktyv == 'slen':
        plt.title('Slenkstinės funkcijos klasifikavimo tikslumas nuo mokymosi greičio')
    else: plt.title('Sigmoidinės funkcijos klasifikavimo tikslumas nuo mokymosi greičio')

    plt.xlabel('Iteracijų skaičius')
    plt.ylabel('Klasifikavimo tikslumas')
    plt.show()
    return

#Testavimas pagal epochu skaiciu (tyrimui)
def test_epoch(mokDuom, testDuom, aktyv, epochu_sk, mok_greit):
    #sukuriame geriausio masyva: epocha, greitis, svoriai(5), klasifik. tikls. (test), paklaida (mok)
    geriausias = np.array([0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,1000.0])
    for greitis in mok_greit:
        greitis = round(greitis, 1)
        #greitis = round(greitis, 2)
        atsakymai = np.empty([1,3])
        atsakymai = np.delete(atsakymai, 0, axis = 0)
        for epocha in epochu_sk:
            svoriai = mokymas(mokDuom, aktyv, 0, epocha, greitis)
            atsak = testavimas(svoriai, testDuom, aktyv)
            atsakymai = np.append(atsakymai, [[atsak, greitis, epocha]], axis = 0)
            #tikriname, ar tai geriausi gauti svoriai pagal testDuom klasifikavimo tiksluma
            if geriausias[7]<atsak:
                geriausias[0] = epocha
                geriausias[1] = greitis
                for i in range(5):  geriausias[i+2] = svoriai[i]
                geriausias[7] = atsak
                geriausias[8] = svoriai[6]
            #jei klas. tiksl. sutampa, tai tikriname, kurio paklaida (mokDuom) yra mazesne 
            elif geriausias[7] == atsak:
                if geriausias[8] > svoriai[6]:
                    geriausias[0] = epocha
                    geriausias[1] = greitis
                    for i in range(5):  geriausias[i+2] = svoriai[i]
                    geriausias[7] = atsak
                    geriausias[8] = svoriai[6]
        plt.plot(atsakymai[:,2], atsakymai[:,0], label = str(greitis))

    plt.legend(title = 'Greičiai')

    if aktyv == 'slen':
        plt.title('Slenkstinės funkcijos klasifikavimo tikslumas nuo mokymosi greičio')
    else: plt.title('Sigmoidinės funkcijos klasifikavimo tikslumas nuo mokymosi greičio')

    plt.xlabel('Epochų skaičius')
    plt.ylabel('Klasifikavimo tikslumas')
    plt.show()

    print('Geriausias rastas variantas: epocha={0},\n mokymosi greitis={1},\n w0={2}, w1={3}, w2={4}, w3={5}, w4={6} \n Klasifikavimo tikslumas (testDuom)={7},\n Paklaida(mokDuom)={8}'.format(
          round(geriausias[0],0), round(geriausias[1],1), geriausias[2], geriausias[3], geriausias[4], geriausias[5],
          geriausias[6], round(geriausias[7],2), round(geriausias[8],3)))
    return

#Main funkcija
def main(args=None):
    #pirma nuskaitome duomenis ir sukuriame naujus failus:
    #duomenu_tvarkyba('iris.data', 'duom1.csv', 'duom2.csv')

    #jei leisime ateityje, galesime iskart skaityti susikurtus failus
    duom1 = pd.read_csv('duom1.csv', sep = ',')
    duom2 = pd.read_csv('duom2.csv', sep = ',')

    #mokymo ir testavimo aibes imsime santykiu 80:20 atitinkamai
    #renkame paprastaja atsitiktine negrazintine imti
    mokDuom1 = duom1.sample(frac=0.8, random_state=419)#
    testDuom1 = duom1.drop(mokDuom1.index)
    mokDuom2 = duom2.sample(frac=0.8, random_state=419)#
    testDuom2 = duom2.drop(mokDuom2.index)  

    #Paleiskime paprasta neurono mokyma (jei norime paklaidos grafiko ar spausdinimo parametru, iveskime True, True)
    #mokymas(mokDuom1, "slen", 0, 30, 0.1, True, True)
    #mokymas(mokDuom1, "sig", 0, 30, 0.1, True, True)
    #mokymas(mokDuom2, "slen", 0, 30, 0.1, True, True)
    #mokymas(mokDuom2, "sig", 0, 30, 0.1, True, True)

    #leidziame paprasta neurono mokyma ir testavima (su testiniais duomenimis), grazina svorius ir klas. tiksl.
    #print(mokTest(mokDuom1, testDuom1, "slen", 100, 0, 0.1))

    #nustatome testavimo iteraciju ir epochu intervalus (6 uzduotis)
    iter_sk = np.arange(start = 10, stop = 510, step = 10)
    epochu_sk = np.arange(start = 1, stop = 31)

    #nustatome testavimo mokymosi greicio intervalus (6 uzduotis)
    mok_greit = np.arange(start = 0.1, stop = 1, step = 0.1)

    #############################################################
    #leidziame tyrima, 6 punktas (atkomentuoti tik po viena eilute per programos paleidima)
    #Pirmi duomenys, slenkstine funkcija
    #   Keiciant iteracijas
    #test_iter(mokDuom1, testDuom1, "slen", iter_sk, mok_greit)
    #   Keiciant epochas
    #test_epoch(mokDuom1, testDuom1, "slen", epochu_sk, mok_greit)

    #Pirmi duomenys, sigmoidine funkcija
    #   Keiciant iteracijas
    #test_iter(mokDuom1, testDuom1, "sig", iter_sk, mok_greit)
    #   Keiciant epochas
    #test_epoch(mokDuom1, testDuom1, "sig", epochu_sk, mok_greit)


    #Antri duomenys, slenkstine funkcija
    #   Keiciant iteracijas
    #test_iter(mokDuom2, testDuom2, "slen", iter_sk, mok_greit)
    #   Keiciant epochas
    #test_epoch(mokDuom2, testDuom2, "slen", epochu_sk, mok_greit)

    #Antri duomenys, sigmoidine funkcija
    #   Keiciant iteracijas
    #test_iter(mokDuom2, testDuom2, "sig", iter_sk, mok_greit)
    #   Keiciant epochas
    #test_epoch(mokDuom2, testDuom2, "sig", epochu_sk, mok_greit)

    #mokTest(mokDuom1, testDuom1, "sig", 0, 2, 0.5)
    mokTest(mokDuom2, testDuom2, "sig", 0, 14, 0.9)

    return



#programos aktyvacijos salyga
if __name__ == '__main__':
    main()