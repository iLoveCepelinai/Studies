#Pypkes (pipes)
library(tidyverse)
library(nycflights13)
#mums reikia magrittr paketo pypkems
#Paipus naudoti kai yra vienas dataframe. Gana trumpa tiesine seka be sakojimusi

flights$dep_delay %>% mean(na.rm=T) %>% '*'(2)#'*' cia kaip funkcija
#pirma flights$dep_delay imamas kaip x i funkcija mean(), tada tai kas gaunama dedama i funkcija *2
# the scenes, x %>% f(y) turns into f(x, y), and x %>% f(y) %>% g(z) turns into g(f(x, y), z) and so on.
flights %>% 
  group_by(year, month, day) %>% 
  summarise(mean = mean(dep_delay))

vid_neatsaukt_skydz_atid_laik <- flights %>% filter(!is.na(dep_delay)) %>%
  group_by(year, month, day) %>% 
  summarise(mean = mean(dep_delay))
vid_neatsaukt_skydz_atid_laik

x<-flights$dep_delay
x %>% '-'(mean(x, na.rm=T)) %>% '^'(2) %>% mean(na.rm = T) #nera jokio sakojimosi (pvz nera jei kazkas daryk vienaip,...)


#Turek omenyje sample funkcija

#naudodami pypkes (nuosekliai)
#sukurit vektoriu su koordinatemis 1,2,..10
#sukurti vektoriu su 30 atsitiktiniu koordincaciu, gautu renkant imti su grazinimu (sample) is 1 zingsnio vektoriaus
#surasyti vektoriu i matrica (turincia tris eilutes)
#atspausdinti kiekvienos eilutes dispersija
#atspausdinti visa dispersiju vektoriu, didziausia dispersija ir eilutes numeri, atitinkanti didziausia var reiksme
#   pasiulymas naudoti funkcija which funkcija

which.max(c(2,2,4,3,4))#grazina pirmo sutikto maksimumo pozicija

f <- function(x){
  print("Dispersiju vektorius:")
  print(x)
  print("Maksimali dispersija ir eil nr.:")
  c(max(x, na.rm = T),which.max(x))
}

c(1:10) %>% 
  sample(size = 30, replace = T) %>%
  matrix(nrow = 3) %>%
  apply(MARGIN = 1, FUN = var, na.rm = T) %>%
  f()

#2 uzduotis
#sugeneruokite vektoriu su koord -10 ... 10
#sumaisykite su funkcija sample
#panaudoti sapply ir pakeisti neigiamas koord ju kvadratais
#sudeti i matrica su viena eilute
#panaudodami apply paskaiciuoti vektoriaus vidurki, dispersija ir imties ploti

c(-10:10) %>%
  sample() %>%
  sapply(FUN = function(x_i){ifelse(x_i<0, x_i**2, x_i)}) %>%
  matrix(nrow = 1) %>%
  apply(MARGIN = 1, FUN = function(eil){
    c(mean(eil,na.rm = T), var(eil, na.rm = T), max(eil,na.rm = T)-min(eil,na.rm = T))
  })

