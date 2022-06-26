# -*- coding: utf-8 -*-

#Matas Amsiejus
#DM2

#Koda galima paleisti (naudosiu 8 branduolius kaip pavizdi):
# *per WINDOWS: mpiexec -n 8 python individuali_uzd.py
# *per LINUX (interaktyviai): salloc --ntasks=8 mpirun python3 individuali_uzd.py
# *per LINUX (naudojant script'a (scripte originaliai 8 branduoliai, bet pakeisti nesunku)): sbatch indiv_uzd_script.sh
#  Duomenys irasomi i slurm tekstini faila.

#Neissiaiskinau priezasties, kodel metamas ispejimas (kazkas su fork'inimu). Jei pavyks suzinoti, praneskit.

#Biblioteka lygiagretinimui
from mpi4py import MPI
#pandas reikalinga duomenu tvarkybai
import pandas as pd
#numpy geresniems masyvams
import numpy as np
#reikes funkcijai floor
import math
#su os keiciu darbo direktorija
#import os

#Tuo atveju jei jums neranda duomenu failo tame paciame aplanke kaip scriptas
#failo_vieta = os.getcwd()
#os.chdir(failo_vieta)
#########################################################################################################
#Duomenu tvarkymas
def duomenu_tvarkyba(duom_pav):
    #nuskaitome iris.data duomenis
    duom = pd.read_csv(duom_pav, header = None, sep = ',')
    #pakeisciame stulpeliu vardus i lietuviskus
    duom.columns = ['taurel_ilg','taurel_plot','ziedl_ilg','ziedl_plot','klase']

    #Sukuriame kopija originalios lenteles naujoms klasems, kur Versicolor bus 0 klaseje, o Virginca - 1
    nduom = duom.copy()
    #Pries tai pasaliname nereikalinga Setosa klase
    nduom.drop(nduom[nduom.klase == "Iris-setosa"].index, inplace=True)
    #Pakeiciame klases i 0 arba 1
    nduom["klase"] = nduom["klase"].replace({"Iris-virginica": 1, "Iris-versicolor":0})

    return nduom

#Dirbtinis neuronas.
def neuronas(svoriai, x1, x2, x3, x4, aktyv):
    #skaiciuojame suma
    a = 1*svoriai[0] + x1*svoriai[1] + x2*svoriai[2] + x3*svoriai[3] + x4*svoriai[4]
    #skaiciuojame pasirinktos aktyacijos f-jos reiksme 
    if aktyv == "slen":
        if a>=0:    return 1
        else:       return 0
    else:
        f = 1/(1+math.e**(-a))
        return f
    
#Dirbtinio neurono mokymas
def mokymas(mokDuom, aktyv, epochu_sk, mok_greit):
    np.random.seed(419)#nustatome, kad atsitiktinumas butu vienodas visoms versijoms
    svoriai = np.random.uniform(size=5)#sugeneruojame 5 atsitiktinius svorius pradziai

    #ciklas, skirtas eiti tol, kol pasiekeme uzsibrezta epochu skaiciu
    for ep_nr in range(epochu_sk):
        #iteruojame per duomenu faila
        for iterac in range(len(mokDuom)):
            #nuskaitome lenteles eilute
            iejimai = mokDuom.iloc[iterac]
            #suskaiciuoajme isvesties reiksme
            y = neuronas(svoriai, iejimai[0], iejimai[1], iejimai[2], iejimai[3], aktyv)
            #naudojame round jei sigmoidine funkcija, tai is intervalo (0,1) taps klase 0 arba 1.
            #Koreguojame svorius jei blogai nustatoma geles klase
            if round(y,0) != iejimai[4]:
                #koreguojame 0-ini (bias) svori
                svoriai[0]=svoriai[0]+mok_greit*(iejimai[4]-y)*1
                #iteruojame per visus geles atributus
                for i in range(4):
                    #koreguojame likusius svorius (w1-w4)
                    svoriai[i+1]=svoriai[i+1] + mok_greit * (iejimai[4]-y)*iejimai[i]
    #graziname apmokyto neurono svorius
    return svoriai

#Dirbtinio neurono testavimas
def testavimas(svoriai, testDuom, aktyv):
    #ivedame klasifikavimo tiksluma
    klas_tiksl = 0
    #einame per testDuom lentele (+-20 % visu duomenu)
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

#Main funkcija
def main(args=None):
    #inicijuojame lygiagretinimo procesa
    comm = MPI.COMM_WORLD
    #nustatome, koks musu rangas
    rank = comm.Get_rank()
    #nustatome, kiek viso bus rangu
    size = comm.Get_size()

    #pirma nuskaitome duomenis ir sukuriame naujus failus (tik jei rangas 0):
    if rank == 0:
        #susitvarkome iris duomenis, kad butu tik dvi geles: versicolor ir virginica. Irasome i duom
        duom = duomenu_tvarkyba('iris.data')
        #permaisome duomenis papildomam atsitiktinumui
        duom = duom.sample(frac=1, random_state = 419).reset_index(drop=True)
        #kopijuojame duomenis visiems procesams
        data = [duom]
        #kopijuojame tiek, kiek bus branduoliu
        for i in range(size-1):
            data.append(duom)
    
    else : data = None
    
    #padaliname / gauname duomenis
    data = comm.scatter(data, root=0)
    
    #nustatome duomenu ilgi (tuo atveju, jei iris.data papilnetu eilutemis)
    duom_ilg = len(data)
    
    #randame rezius, kur reikes dalinti duomenis pagal branduoliu skaiciu
    gaps = np.round(np.linspace(0, duom_ilg, size+1), 0)
    #nustatome "bloko" didi - kokia dalis duomenu (jei nieko neredaguociau) tektu mokymui
    blok_dyd = int(duom_ilg / size)


    #testiniu duomenu turi buti bent 20 %, todel jei bus maziau, jungsime blokus
    #pvz musu redaguota iris turi 100 eiluciu. Jei paleisime su 8 branduoliais, musu blokas bus < 20%
    #100/8=12,5 < 100. Todel reiks apjungti kelis blokus.
    if blok_dyd < 0.2 * duom_ilg:
        #kiek_blok pasakys kiek bloku teks imti testavimui. Jei originaliai blok_dyd >= 20 %, tai kiek_blok=1
        kiek_blok = 1
        #tol kol testiniu duomenu nebus bent 20%, tol didinsime kiek_blok
        while kiek_blok*blok_dyd < 0.2 * duom_ilg:
            kiek_blok += 1
        #gauname kintamaji kiek_blok - kuris pasakys, kiek bloku reikia apjungti

        #kol musu ribos neissoka is duomenu pabaigos, tol vykdome iprastai
        if (rank + kiek_blok) <= size:
            test_duom = data.iloc[int(gaps[rank]):int(gaps[rank+kiek_blok])]
        #taciau kai tik musu galutine riba yra daugiau uz duom pabaiga, tai imsime duomenis is priekio
        else:
            #like duomenys is galo
            test_duom1 = data.iloc[int(gaps[rank]):int(gaps[size])]
            #duomenys jau nuo priekio
            test_duom2 = data.iloc[0:int(gaps[rank+kiek_blok-size])]
            #sujungiame duomenis
            test_duom = pd.concat([test_duom1, test_duom2], axis = 0)

    #jei blok_dyd >=20%, viskas paprastai (imame duomenis pagal programos ranga)
    else:
        test_duom = data.iloc[int(gaps[rank]):int(gaps[rank+1])]
    
    #like duomenis (ne testavimo) bus mokymo duomenys
    mok_duom = data.drop(test_duom.index)

    #apmokome neurona (siuo atveju f-ja sigmoidine, bus 10 epochu ir mokymo greitis = 0.1)
    svoriai = mokymas(mok_duom, "sig", 10, 0.1)
    #testuojame neurono tiksluma
    klasifik_tiksl = testavimas(svoriai, test_duom, "sig")


    #Spausdiname
    print('Procesas {} suskaiciavo tokius svorius:'.format(rank), np.round(svoriai, 3))
    print('Gautas {}-o proceso klasifikavimo tikslumas:{}'.format(rank, np.round(klasifik_tiksl, 2)))

    #Surenkame visus rezultatus is visu procesu
    rez = comm.gather(klasifik_tiksl, root=0)

    #Suskaiciuojame klasifikavimo vidurki 0 range is visu procesu mokymo / testavimo
    if rank == 0:
        vid = round(np.mean(rez),5)
        print("\n Vidutinis kryzminio klasifikavimo tikslumas: {}".format(round(vid,3)))
    return


#programos aktyvacijos salyga
if __name__ == '__main__':
    main()