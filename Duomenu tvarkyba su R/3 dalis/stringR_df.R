library(tidyverse)

#NAUDINGI LINKAI:
#https://www.petefreitag.com/cheatsheets/regex/character-classes/
#https://www.regular-expressions.info/refshorthand.html

#str_detect
#\\b(T|t)he
sentences[str_detect(string = sentences, pattern ="\\b(T|t)he\\b")]

# How many common words start with t?
sum(str_detect(words, "^t"))
# Find all words containing at least one vowel, and negate
no_vowels_1 <- !str_detect(words, "[aeiou]")
words[no_vowels_1]

words[str_detect(words, "x$")]
str_subset(words, "x$")

#Kokiame zodyje a pasitaiko dazniausiai
x <- c("apple", "banana", "pear")
str_count(x, "a")

df<-tibble(word = words,
           i = seq_along(word),
           a_count = str_count(words, "a"))
df%>%arrange(desc(a_count))


df %>% mutate(
    vowels = str_count(word, "[aeiou]"),
    consonants = str_count(word, "[^aeiou]")
  )


#dar naudingu funkciju:
#str_replace()
#str_replace_all()
#str_split() (naudinga boundary funkcija)
#str_locate()

##########################
#Pratybos
#Naudosime fruits
#1. Ar yra vaisiu, prasidedanciu w raide?
fruit %>%
  str_to_lower() %>%
  str_subset(pattern = "^w")

#2. Atrinkite vaisius, kuriu pavadinimai prasideda priebalse ir baigiasi ppriebalse
fruit %>%
  str_to_lower() %>%
  str_subset(pattern = "^[^aeiouy](\\w|\\s){0,}[^aeiouy]$")#w reiskia zodis (nera tarpo)
#arba
l1 <- fruit %>% str_to_lower() %>%
  str_detect(pattern = "^[^aeiouy]")

l2 <- fruit %>% str_to_lower() %>%
  str_detect(pattern = "[^aeiouy]$")

fruit[l1&l2]


#3. Atrinkite vaisius, kuriu pavadinimuose ta pati raide pasikartoja:
#a) lygiai du kartus is eiles (apple);
f <- function(w){
    raides <- str_extract_all(string = w, pattern = "[:lower:]")[[1]]
    return(sum(sapply(X = raides, FUN = function(raide){
    x<-str_locate_all(w, pattern = "a")[[1]][,1]
    n<-length(x)-1
    y <- sapply(X=c(1:n), FUN = function(i){x[i+1]-x[i]})
    return (length(y[y==1])==1)
  }))>0)
}
f("labaas")
fruit[sapply(fruit, f)]
fruit[f(fruit)]
#PErkopijuok nuo destytojo kodo

str_extract_all(string = "jonas", pattern = "[:lower:]")[[1]]

x<-str_locate_all("labasaaa", pattern = "a")[[1]][,1]
n<-length(x)-1
y <- sapply(X=c(1:n), FUN = function(i){x[i+1]-x[i]})
y[y==1]
#b) du arba daugiau kartu ne is eiles


# 4. Kiek yra vaisiu, kuriu pavadinime balsiu sk. virsija prieb. sk.?
df <- tibble(fruit=fruit) %>%
  mutate(balsiu_sk = str_count(fruit, pattern = "[aeiouy]"),
         priebalsiu_sk = str_count(fruit, pattern = "[^aeiouy ]")) %>%
  filter(balsiu_sk>priebalsiu_sk)
df

str_count("abba", "a")


# 5. Kiek zodziu, kuriu pavadinimai is dvieju zodziu?
#universalus variantas
x <- fruit %>% str_split(pattern = boundary("word")) %>%
  sapply(FUN = length) %>% tibble()
length(x[x==2])

#simple kaka
fruit %>% str_detect(pattern = "\\s{1}") %>%
  sum()

#trecias prendimas fml
fruit %>% str_detect(pattern = "^\\w{1,}\\s\\w{1,}$") %>%
  sum()

str_split(string = "Labas Jonai, kaip laikaisi?", pattern = boundary("word"))
str_split(string = "Labas Jonai, kaip laikaisi?", pattern = boundary("character"))



#nzn kas cia darbas su sentences
#1. Kuris sakinys ilgiausias, jei ilgi matuojame:
#a) zodziu sk.
f <- function(s, splitBy){
  zodziai <- str_split(string = s, pattern = boundary(splitBy))[[1]]
  return(length(zodziai))
}


tibble(s=sentences) %>%
  mutate(z_sk=sapply(X=s, FUN = f, splitBy="word"),
         sim_sk=sapply(X=s, FUN = f, splitBy="character"))

#2. Isrinkti visus senteces panaudotus skirybos zenklus
str_extract_all(string = str_c(sentences, collapse = " "), pattern = "[:punct:]")[[1]] %>% 
  unique()

#3. Isrinkti sakinius, kuriuose be skaini baigiancio skirybos zenklo daugiau jokiu kitu 
#   skirybos zenklu nera (arba kiek ).
str_sub("labas", start = 1, end = c(-2))

tibble(s=str_sub(string=sentences, start = 1, end = c(-2))) %>%
  mutate(sk_zenklu_sk=str_count(string = s, pattern = "[:punct:]")) %>%
  filter(sk_zenklu_sk>0)
