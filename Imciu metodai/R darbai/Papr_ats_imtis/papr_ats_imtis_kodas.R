# PVZ 1 - pirma apdorojame duomenis naudodami paprasata R koda

# nuskaitome duomenis is failo:
duom <- read.csv("islaidos_gamtos apsaugai.txt", sep="")
# sukuriame kintamji, kuris bus imties dydis (n)
didumas<-20
# sudarome atsitiktine imti imtis_imones is 20 elementu is duomenu duom (negrazintine)
imtis_imones<-duom[sample(1:nrow(duom), didumas, replace=FALSE),]
# randame emimo tikimybe (elemento priklausymo imciai tikimybe) pi_k = n/N (tikimybe gaunasi ta pati, nes tai 
# paprastoji atsitiktine imtis)
imtis_imones$EmimoTikimybe <- 20/84
# randame emimo svori (imties plano svori)
imtis_imones$EmimoSvoris <-1/imtis_imones$EmimoTikimybe
imtis_imones


# Pritaikome SURVEY paketa apdorotiems duomenims

library(survey)
# apsibreziame plana (ids = ~1 - imami imties elementai (ne grupes kaip butu lizdiniame), data = ..(dataframe objektas),
# svoriai (weights)) nebutini, fpc - butinas, jei nori kad butu pritaikyta baigtines populiacijos pataisa (negrazintines
# atveju). Reikia N arba n/N). Jei nezinai kokio dydzio visa populiacija, tai susumuok svorius (weights) - 
# sum(imtis_imones$EmimoSvoris)
imties_planas <- svydesign( ids = ~1 , data = imtis_imones, weights = ~EmimoSvoris, fpc = ~EmimoTikimybe)
# populiacijos parametrai jeigu vidurkis (deff=True parodo imties efekta (lyginama su paprastaja atsitiktine imtimi,
# bet kadangi cia ir yra ji tai gaunasi =1))
svymean(~Pajamos, imties_planas,deff=TRUE)
# populiacijos parametrai jeigu suma
pajamos_suma_rez=svytotal(~Pajamos, imties_planas)
# koeficientas??
coef(pajamos_suma_rez)
# pasikliovimo intervalas
confint(pajamos_suma_rez)


##############################################################3

# PVZ 2

# Bendras kodas

library("readxl")

# nuskaitome imti (nes uzduotyje jau iskart pateikta imtis, populiacija nezinoma)
rinkimai<-read_excel("rinkimai.xls")


# R fromules kodas:

# irasome kintamuosius N, n, Q (pasikliovimo lygmuo)
N<-676;n<-50;Q<-0.8

# skaiciuojame sumos ivertini (t^ (t su ^)). Formule: t^_y = N*(y-) (y- => y su bruksneliu (imties vid.))
imties_vid<-mean(rinkimai$parasu_sk);imties_vid #(y-)
sum_ivert<-N*imties_vid;sum_ivert#t^_y

# dabar ieskosime imties ir statistikos - (O su apatiniu _ (t^_), O su virsutiniu -(t^-))
# t^_=t^-z_(a/2)sqrt(D^t^). Pirma ieskome sqrt(D^t^) - saknies is dispersijos
stnuok<-sqrt((N**2)*(1-n/N)*sd(rinkimai$parasu_sk)**2/n);stnuok # 1392
# dabar reikia rasti z_(a/2). a=1-Q=0.2. z_(a/2)=z_0.1. Sita skaiciuojame su R:
z<-qnorm(0.1, lower.tail = FALSE);z
t_apat<-sum_ivert-z*stnuok;t_apat
t_virsut<-sum_ivert+z*stnuok;t_virsut

# R paketas SURVEY:
# cia jau imtis, tai nereikia sample naudoti
rinkimai$N<-676
# apibreziame plana
rinkimai_planas <- svydesign(ids = ~1 , data = rinkimai, weights = ~svoris, fpc = ~N)
# populiacijos param jei suma
skaicius <- svytotal(~parasu_sk, rinkimai_planas);skaicius
confint(skaicius, level=0.8)
# Apskaièiuoja pasikliovimo intervalà vidurkiui naudojant Stjudento skirstinio kvantilius ir atsiþvelgiant á 
# baigtinës populiacijos pataisà
library(jmuOutlier)
676*CI.t.test(rinkimai$parasu_sk, conf.level = 0.8, fpc = 1-50/676)

################################################################################

# R1 uzduotys

# 1. ([1], 3.5.1). Pastaba. Uþduotá a) atlikite R uþprogramavæ formules; b) naudodami paketà survey.

# Kaime yra 100 namø ûkiø. Ið ðios populiacijos iðrinkta 10 namø ûkiø paprastoji atsitiktinë imtis. Á jà patekusiuose
# namø ûkiuose gyvena 2, 5, 1, 4, 4, 3, 2, 5, 2, 3 þmoniø.
# a) Ávertinkite vidutiná namø ûkio nariø skaièiø kaime ir ðio ávertinio dispersijà.
# b) Ávertinkite gyventojø skaièiø kaime ir ðio ávertinio dispersijà.

# DARANT NAUDOJANT R PROGRAMUOJANT FORMULES
N=100;n=10
duom<-c(2, 5, 1, 4, 4, 3, 2, 5, 2, 3);duom
rez<-data.frame(zmones=duom)
# a) rasti pagal vidurki. Taigi pirma randame imties vidurki (y-):
imties_vid<-mean(rez$zmones);imties_vid # taciau tai yra ir populiacijos vidurkis, nes yra tik 1 imtis imties plane
# Taip pat reikia rasti ivertinio dispersija (s^)**2
ivert_disp=(1-10/100)*var(rez$zmones)/n;ivert_disp
# b) rasti pagal suma
pop_sum_vid<-imties_vid*N;pop_sum_vid
ivert_sum_disp<-(N**2)*ivert_disp;ivert_sum_disp
var(duom)
# DARANT NAUDOJANT PAKETA SURVEY
rez1<-rez
rez1$N<-100
ukiai_planas <- svydesign(ids = ~1 , data = rez1, fpc = ~N)
# a)
svymean(~zmones, ukiai_planas) # bet cia pateikia sd vietoje var, tad reikia kazka daryti???
# b)
svytotal(~zmones, ukiai_planas)


# 2. ([1], 3.5.5) Mieste yra 20 viduriniø mokyklø. Atsitiktinai atrinktose keturiose mokyklose yra toks mokiniu 
# skaièius (lentele mokyklos.txt). Laikydami, kad mokyklø imtis yra paprastoji atsitiktinë, ávertinkite mokiniø skaièiø
# 1-4 klasëse, 5-8 klasëse, 9-12 klasëse. Ávertinkite bendrà visø miesto mokyklø mokiniø skaièiø ir visø ávertiniø 
# dispersijas ir variacijos koeficientus.
duom <- read.csv("mokyklos.txt", sep="\t");duom
names(duom)<-c("mokykla", "1-4 kl", "4-8 kl", "9-12 kl");duom
N<-20; n<-4
#DARANT NAUDOJANTIS R FORMULEMIS
#Vertiname mokiniu skaiciu atitinkamose klasese
skaicius1_4<-mean(duom$`1-4 kl`)*N;skaicius1_4
skaicius5_8<-mean(duom$`4-8 kl`)*N;skaicius5_8
skaicius9_12<-mean(duom$`9-12 kl`)*N;skaicius9_12
# Visas mokiniu skaicius mieste
bendras_sk<-N*(mean(duom$`1-4 kl`)+mean(duom$`4-8 kl`)+mean(duom$`9-12 kl`));bendras_sk
# Vertinamos ivertiniu dispersijos ir variacijos koef
disp1_4<-N^2*(1-4/20)*(1/n)*(1/(n-1))*var(duom$`1-4 kl`)*N;disp1_4#ohhh dude ima pagal nutylejima n-1 asileeee
#cv1_4<-sqrt(disp1_4)/???
# DARANT DAUDOJANTIS PAKETU SURVEY
