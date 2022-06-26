#Kontrolinis 3
#Matas Amšiejus
#DM 2

library (tidyverse)

########################
# 1 uzduotis
# a)

testStr_a <- c("\\^ +","\\^++ labas","cia sablono nera")
writeLines(testStr_a)#writeLines tik pasitikrinimui

writeLines("\\\\\\^ \\+")
str_view_all(string = testStr_a, pattern = "\\\\\\^ \\+")


#b)
testStr_b <- c("www.delfi.lt","www.pigu2.lt www.gmail.com",
               "www.pigu2.lt-sito-neimti ir www.-sito_neimti.lt")

str_view_all(string = testStr_b, pattern = "\\bwww\\.[:alpha:]*\\.lt\\b")

#c)
testStr_c <- c("labas, Jonai",
               "vabalas ir tuktuk tinka c2, o abbba c1")
# c1)
str_view_all(string = testStr_c, pattern = "\\b[:alpha:]([:alpha:])([:alpha:]){2}\\1\\b")
#sukurkime nauja stringa su kuriuo veiks (antra raide sutampa su paskutine) - kakta: 
str_view_all(string = "kakta, tomas, babbaa",
             pattern = "\\b[:alpha:]([:alpha:])([:alpha:]){2}\\1\\b")

# c2)
str_view_all(string = "kakta, tomas, babbba",
             pattern = "\\b[:alpha:]([:alpha:])([:alpha:]){0,}\\1\\b")
#su duotu pavyzdziu:
str_view_all(string = testStr_c, pattern = "\\b[:alpha:]([:alpha:])([:alpha:]){0,}\\1\\b")

####################
#2 uzduotis
testas <- c("1998-12-09 Pietu Afrikoje rasti mazdaug prieš 3,6 mln. metu gyvenusio zmogaus griauciai – seniausias toks radinys.",
  "2000 m. geguzes 24 d. (24/09/00) Izraelis uzbaige 22 metus trukusia Pietu Libano okupacija.",
  "1994 05 09 	LR pasirase asocijuota sutarti su Europos Sajunga.",
  "04'01'1643 gime Izaokas Niutonas. 04'01'2021 jo sukurta teorija buvo ir toliau destoma aukstosiose mokyklose.")


datos <- function(tekstai){
  tink_datos_tekste <- c()
  for (i in 1:length(tekstai)){
    sakinys <- tekstai[i]
    tink_datos <- str_extract_all(sakinys, pattern = "\\b(((20[0-9][0-9])|(19[0-9][0-9]))(/|-)(([0][1-9])|([1][0-2]))(/|-)((0[1-9])|([12][0-9])|(3[01])))|(((0[1-9])|([12][0-9])|(3[01]))(')(([0][1-9])|([1][0-2]))(')((20[0-9][0-9])|(19[0-9][0-9])))\\b")
    tink_datos_tekste <- c(tink_datos_tekste, tink_datos)
  }
  return (tink_datos_tekste)
}

sak<-"1998-12-09 Pietu Afrikoje rasti mazdaug 2099/12/09 prieš (09-12-2099) 3,6 mln. metu gyvenusio zmogaus griauciai – seniausias toks radinys."

datos(testas)


######################
#3 uzduotis
rask_ilga <- function(sakinys){
  sarasas <- str_split(sakinys,pattern = boundary("word"))
  sapply(sarasas[1], FUN = nchar)
}

sapply(sentences, FUN = rask_ilga)


#######################
#4 uzduotis
library(stringi)
stri_rand_strings(n = 10, length = c(2,8))
