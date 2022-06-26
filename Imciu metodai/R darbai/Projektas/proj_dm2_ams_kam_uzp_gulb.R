library(readr)
library(survey)
library(sampling)
parkas<-read_csv("Atviri_TP_parko_duomenys.csv")
lengvi<-subset(parkas, parkas$KATEGORIJA_KLASE=="M1", select = c(1, 17, 21, 28, 31, 52, 54, 55, 59, 60, 65, 70, 71))
masinos<-subset(lengvi, !is.na(lengvi$NUOSAVA_MASE) & !is.na(lengvi$GALIA) & !is.na(lengvi$RIDA) & 
                  !is.na(lengvi$MAKS_GREITIS) & !is.na(lengvi$PIRM_REG_DATA))

masinos$Metai<-strtoi(substring(masinos$PIRM_REG_DATA,1,4))

#Populiacijos dydis

#1
#Norime nustatyti sluoksniu dydzius. 
#Tam naudosime saknies is sukauptuju dazniu metoda (Dalenius & Hodges).
#Pirma skirstome musu visa amziu i intervalus po 5 (metus) ir skaiciuojame pasitaikiusiu elementu kieki juose:
dazniai<-c()
for(i in seq(1950, 2020, 10)){
  daznis<-nrow(subset(masinos, Metai>=i & Metai<(i+10)))
  dazniai<-c(dazniai, daznis)
}
#randame dazniu saknis
sak_dazniai<-sqrt(dazniai)
#randame sukauptuju dazniu saknis
suk_dazniai<-cumsum(sak_dazniai)
#musu norimos ribos:
riba<-suk_dazniai[length(suk_dazniai)]/3;riba
riba*2
suk_dazniai
#gauname, kad imant kas 10 metu ir norint suskirstyti i tris panasaus dydzio intervalus, gauname, kad automobiliai iki
#2000 metu pakliuna i pirma intervala, iki 2010 - antra, o visi velesni - 3
masinos$sluoksniai<-ifelse(masinos$Metai<2000, 1, ifelse(masinos$Metai<2010, 2, 3))


#2
#Tarkime galime imti 5000 masinu imti (n=5000). Reikia rasti n_h (h=1,2,3), t.y. kiekvieno sluoksnio imties dydi
#Sluoksniu imtis skirstysime proporcingai pagal sluoksnio diduma:
n<-5000
N<-nrow(masinos);N
N1<-nrow(subset(masinos, masinos$sluoksniai == 1))
n1<-round(n*N1/N);n1

N2<-nrow(subset(masinos, masinos$sluoksniai == 2))
n2<-round(n*N2/N);n2

N3<-nrow(subset(masinos, masinos$sluoksniai == 3))
n3<-round(n*N3/N);n3

n1+n2+n3

#3
#Randame emimo svorius
masinos$svoris <- ifelse(masinos$sluoksniai == 1, N1/n1, ifelse(masinos$sluoksniai == 2, N2/n2, N3/n3))
#Planui reikes populiacijos sluoksnio dydzio, taigi priskiriame:
masinos$N_h <- ifelse(masinos$sluoksniai == 1, N1, ifelse(masinos$sluoksniai == 2, N2, N3))


#4
#Renkame paprastaja atsitiktine sluoksnine imti
#Pirma surusiuojame elementus pagal sluoksni
sorted_masinos <- masinos[order(masinos$sluoksniai),]
#Randame elemnto patekimo i imti tikimybe
sorted_masinos$pik <- inclusionprobastrata(sorted_masinos$sluoksniai, c(n1,n2,n3))
#Atrenkame indikatorine imti, kuri nurodo duomenu indeksus (su id)
pag_imtis_duom <-sampling::strata(sorted_masinos, stratanames=c("sluoksniai"), c(n1,n2,n3), method="srswor",
                                  sorted_masinos$pik, description = TRUE )
#konvertuojame pagalbine imti i duomenis
imtis_duom <-getdata(sorted_masinos, pag_imtis_duom)
imtis_duom$sk<-1

#5
#Sukuriame imties plana
plan_sluoks_masinos<-svydesign(ids = ~1, strata = ~sluoksniai, weights = ~svoris, data = imtis_duom, fpc = ~pik)

#6
#Ieskome parametru ivertiniu
#Galios vidurkis
galios_vid<-svymean(~GALIA, plan_sluoks_masinos, deff=T);galios_vid
confint(galios_vid)#pasikliovimo intervalas (95%)
cv(galios_vid)#variacijos koeficiento ivertinys
#maksimalaus greicio vidurkis
greic_vid<-svymean(~MAKS_GREITIS, plan_sluoks_masinos, deff=T);greic_vid
confint(greic_vid)
cv(greic_vid)
#vidutine mase pagal kebulo tipa
mase<-svyby(formula = ~NUOSAVA_MASE, by = ~KEB_PAVADINIMAS, design = plan_sluoks_masinos, FUN = svymean);mase
confint(mase)
#vidutine automobiliu rida
rida<-svymean(~RIDA, plan_sluoks_masinos, deff=T);rida
confint(rida)
cv(rida)
rida_kuras<-svyby(~RIDA,~DEGALAI,plan_sluoks_masinos,svymean);rida_kuras
rida_vietos<-svyby(~RIDA,~SEDIMU_VIETU_SK,plan_sluoks_masinos,svymean);rida_vietos

#masinu skaicius
svytotal(~sk, plan_sluoks_masinos)
svyby(~sk,~MARKE,plan_sluoks_masinos,svytotal)

#masinos pagal degalus
svyby(~sk,~DEGALAI,plan_sluoks_masinos,svytotal)
#palyginimui galime paskai?iuoti realius populiacijos duomenis
table(masinos$DEGALAI)


#vertiname automobiliu skaiciu pagal kebuki tipa
sk_kebulas<-svyby(~sk,~KEB_PAVADINIMAS, plan_sluoks_masinos, svytotal);sk_kebulas
