#Kontrolinis 2
#Matas Amsiejus, DM2

library(tidyverse)
library(DescTools)

#Nuskaitome duomenis
df<- diamonds

#1 uzduotis
subDf <- df %>% select(x, y, z)%>% #pasirenkame stulpelius
  rename(X = x, Y = y, Z = z)%>% #pakeiciame pavadinimus
  mutate(prod = ifelse(X*Y*Z>40, X*Y*Z, 30))%>% #pridedame nauja stulpeli
  filter(row_number() %% 2 == 0)%>% #isrenkame lygines eilutes
  summarise(X_vid = mean(X, na.rm = T),
            Y_disp = var(Y, na.rm = T),
            Z_min = min(Z, na.rm = T),
            mean_abs_dev = MeanAD(prod))#galima su formule arba funkcija is paketo DescTools

#2 uzduotis
#a)
df %>% select(color) %>%#pasirenkame tik spalvu stulpeli
  group_by(color) %>% #sugrupuojame pagal spalva
  mutate(kiekis = n()) %>% #sukuriame nauja stulpeli kur randa kiek elem. grupeje
  unique() %>% #nufiltruojame tik unikalius
  arrange(desc(kiekis)) %>% #isrikiuojame kiekio mazejimo tvarka
  head(1) #imame tik pirma vieta (daugiausia deimantu)

#su summarise fja
df%>%select(color) %>%
  group_by(color) %>%
  summarise(kiekis = n()) %>%#randa elementu grupese kiekius
  arrange(desc(kiekis)) %>% #isrikiuojame kiekio mazejimo tvarka
  head(1)

#b)
df %>% filter(cut == 'Premium') %>%#atrenkame tik premium pjuvius
  select(carat)%>%
  arrange(desc(carat)) %>%
  head(3)

#c)
df %>% select(cut, x) %>%
  group_by(cut) %>%
  mutate(l_max = max(x, na.rm = T)) %>%#randame maksimalia reiksme grupeje
  filter(x <= l_max/2) %>%#atrenkame tik salyga tenkinacius deimantus (<=lmax/2)
  mutate(maz_uz_maxis2 = n())%>%
  select(cut, l_max, maz_uz_maxis2)%>%
  unique()

#su summarise fja
df %>% select(cut, x) %>%
  group_by(cut) %>%
  mutate(l_max = max(x, na.rm = T)) %>%
  filter(x <= l_max/2) %>%
  summarise(kiekis = n())


#d)
df %>% select(clarity, x, y) %>%
  group_by(clarity) %>%
  mutate(sant = x/y) %>%
  mutate(vid_sant = mean(sant, na.rm = T)) %>%
  select(clarity, vid_sant) %>%
  unique()

#su summarise
df %>% select(clarity, x, y) %>%
  group_by(clarity) %>%
  mutate(sant = x/y) %>%
  summarise(sant_vid = mean(sant, na.rm = T))


#3 uzduotis
uzd3 <- df %>% select(price) %>%
  mutate(q25 = quantile(price, prob = 0.25),#apskaiciuojame kvartilius
         q50 = quantile(price, prob = 0.50),
         q75 = quantile(price, prob = 0.75)) %>%
  mutate(diamondCat = ifelse(price<q25, 'C', ifelse(price>q75, 'A', 'B')))
#priskiriame elementams klases A B C

#4 uzduotis
df %>% slice(1:10000) %>% #atrenkame pirmas 10000 eiluciu
  slice_sample(prop = 0.2) #atrenkame 20 % (2000) eilusiu atsitiktinai