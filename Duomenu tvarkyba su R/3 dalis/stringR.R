library(tidyverse) #reikes stringr is tidyverse

#Basic matches

x <- c("apple", "banana", "pear")
str_view(x, "an")#tik pirmas pasitaikymas
str_view_all(x, "an")#visi pasitaikymai

str_view(x, ".a.")#ieskomi zodziai, kur yra a ir aplink bet kokie simboliai (. = bet koks sim)

#naudingi duomenu failai:
words
sentences

str_view(sentences, "\\.")#R reikia 2 backslashu (vienas sako kad nera specialus simbilis,
  #kitas reiskia kad jau reiskia escapinimas)


#anchors

#^ reiskia stringo pradzia, $ - pabaiga
x <- c("apple", "banana", "pear")
str_view(x, "^a")


x <- c("apple pie", "apple", "apple cake")
str_view(x, "^apple$")
str_view(x, "^a...e$")


#simboliu klases

str_view(c("abc", "a.c", "a*c", "a c"), ".[*]c")
str_view_all(words, "\\d")#paryskins skaiicius

zodziai <- c("as", "tu", "ermitazas")
str_view_all(zodziai, "[^a,e]")

str_view(sentences, "[.]$")#neburina su \, galima ir su []

str_view(c("grey", "gray"), "gr(e|a)y")#or salyga kombinuota


#kartojimasis
x <- "1888 is the longest year in Roman numerals: MDCCCLXXXVIII"
str_view(x, "CC?")#lygiai CC
str_view(x, "CC+")#CC arba daugiau
str_view(x, 'C[LX]+')
str_view(x, 'C{2}')
str_view(x, 'C{2,}')#2 arba daugiau kartu (zr. vadoveli, jis geras)

#simboliu grupes (backrefrences)
str_view(fruit, "(..)\\1", match = TRUE)#2 bet kokie simboliai kartojasi 2 kartus
x<-c("abba", "labas", "kuskusas", "musmusas")
str_view(x, "(..)(.)\\1\\2", match = T)

#parasykite sablona, kuris ieskotu 5 simboliu palindromo (nuo priekio ir galo tas pats)
zodziai <- c("civic", "kebab", "level", "ilgaszodis")
str_view(zodziai, "(.)(.)(.)\\2\\1", match = T)

###########################################
# PRATYBOS
###########################################
# Butu gerai kad atsidarytum reguliariuju israisku klases is karto
# Nusikopnk is jo ikelto R failo

# 1. Parasykite reg. israiskas zemiau nurodytu framentu paieskai
#a) raides a,po kurios seka bent viena raide b
testStr_a <- c("labas", "Jonai", "abba", "ragai", "babbbubas")

str_view_all(string=testStr_a,pattern = "ab+") #+ reiskia, kad paskutine raide gali eiti 1 ar daugiau kartu
str_view_all(string=testStr_a,pattern = "ab{1,}") #gali pasikartoti nuo {x,y} x iki y (jei tuscia tai belenkiek)

#b) raides a, po kurios seka bent vienas skaitmuo arba didzioji raide (kaip pasikeistu sprendimas,
#   jei po a raides galetu buti tik skaitmenys arba tik didziosios raides);
testStr_b <- c("a12", "a1i", "aX2", "a2XX", "AXX", "aVB")
str_view_all(string = testStr_b, pattern = "a[0-9A-Z]+")
str_view_all(string = testStr_b, pattern = "a([0-9]+\\b|[A-Z]+\\b)")#noriu kad butu a raide o po jos arba viena seka arba kita. o \\b reiskia boundary klase (pasibaigia zodis)

#c) klaustuko fragmento pabaigoje (kaip pasikeistu sprendimas, jei po klaustuko butu leidziamas dar vienas
#   2 simboliai is tokios aibes [?!.]);
testStr_c <- c("Kvk?", "nesigavo?!", "Ar tikrai.?>>", "O taip????")
str_view_all(string = testStr_c, pattern = "[?]$")#? specialus simbolis, tai arba dek \\? arba [?]
str_view_all(string = testStr_c, pattern = "\\?$")
str_view_all(string = testStr_c, pattern = "\\?[?!.]{1,}$")
#d) fragmentu, apibrezianciu skaiciu daugyba, kai daugybai zymeti naudojama *;
testStr_d <- c("2*1=2; 2*2=4 ir 2^2 = 4, nors 3*3=9")
str_view_all(string = testStr_d, pattern = "[0-9]\\s*[*]\\s*[0-9]+\\s*=*")
#e) specialiu simboliu & . ' ` {  | } paieskai
testStr_e <- "Aibe A={&,.}, o aibe B = {\\,|\'``}"
str_view_all(string = testStr_e, pattern = "[&.'`{|}]")

#2.  Pazymeti dvejetainius skaicius
testStr<-c("0","1"," 00", "10", " 111010 101010111", "jis", "l0va", "01 lol")
str_view_all(string = testStr, pattern = "\\b[01]+\\b")

#3. Rasti IP adresa a.b.c.d, kur kiekviena raide atitinka 1,2 arba 3 skaiciu seka

testStr<- c("1.001.12.3 - pirmas IP", "021.354.120.31 - antras IP",
            "XX1.001.12.322 - ne IP")
str_view_all(string = testStr, pattern = "\\b[0-9]{1,3}[.][0-9]{1,3}[.][0-9]{1,3}[.][0-9]{1,3}\\b")
str_view_all(string = testStr, pattern = "\\b([0-9]{1,3}[.]){3}[0-9]{1,3}\\b")


#4. Kokius sablonus atitinka sios reguliarios israiskos? Kiekvienai pateikite po pavyzdi, 
#   kuris atitinka ir neatitinka nurodyta sablona
#a) [A-Z][0-9][A-Z]\s[0-9][A-Z][0-9]
#Israiskos, kuriam sablonas tinka
str_view_all(string = c("A1B\t0C8", "C2B 0C9"), pattern = "[A-Z][0-9][A-Z]\\s[0-9][A-Z][0-9]")
#Israiskos, kurios netinka:
str_view_all(string = c("A1B\t\t0C8", "C2B.0C9"), pattern = "[A-Z][0-9][A-Z]\\s[0-9][A-Z][0-9]")

#b) ([0-9]|[A-F])+
#Israiskos, kuriam sablonas tinka
str_view_all(string = c("A1B0C8", "66639"), pattern = "([0-9]|[A-F])+")
#Israiskos, kuriam sablonas netinka
str_view_all(string = c("baaaa", "lmao."), pattern = "([0-9]|[A-F])+")

#c) (\(\d\d\d\)\s)?\d\d\d-\d\d\d\d
#tinkamos
str_view_all(string = c("(123) 456-1234", " 456-1234 "), pattern = "(\\(\\d\\d\\d\\)\\s)?\\d\\d\\d-\\d\\d\\d\\d")
writeLines("(\\(\\d\\d\\d\\)\\s)?\\d\\d\\d-\\d\\d\\d\\d") #pasitikrinimui
#netinkamos
str_view_all(string = c("(123)\t\t456*1234"), pattern = "(\\(\\d\\d\\d\\)\\s)?\\d\\d\\d-\\d\\d\\d\\d")

#5. Parasykite sablona, kuris sakinio gale zymetu zodi kartu su sakiny baigianciu skirybos
#   zenklu

testStr <- sentences[1:5]
testStr

#Sprendimas
str_view_all(string = testStr, pattern = "\\b[:word:]+[:punct:]$")

#6. Atrinkti laika

testStr <- c("12:34", "00:00", "12:32 pm", "00:60", "4", "-4:00")

#Sprendimas
timePattern <- "\\b((0[0-9])|(1[0-9])|(2[0-3])):((0[0-9])|([1-5][0-9]))\\b"
str_view_all(string = testStr, pattern = timePattern)


#7. DNA sekos koduojamos  simboliais {A,C,G,T}. SAlyga R faile jo, nusikopink as bbd
DNASekos<-("CCCATGCCCCCTAGCCC")

str_view_all(string = DNASekos, pattern = "\\b(ATG)([ACGT]{3})+((TAA)|(TAG)|(TGA))")


#8. Ar pateiktas pasto fragmentas yra pasto adresas?
#Israiska: [a-z]*@([a-z])+(.[a-z]+)+
#Pirma tik mazosios raides, gali buti didziosios. ten kur . yra bet koks simbolis, reikia 
# \\. ,. Tada kartojasi ta vieta su tasku vis. Galima rasyt @gmail.com.gmail.com, taip pat
#turi kartotis kelis kartus .zodis.zodis, tai paprastas gmail netiktu


#9. \b(\w+)\s+\1\b skirtas surasti dukart pakartotus zodzius. Issiaiskinti spec. simboliu reiksme
str_view_all(string = c("labas\tlabas, Jonai...", "the the cat took the the hat"),
             pattern = "\\b(\\w+)\\s+\\1\\b")

#10. Parasykite reguliaria israiska, kuriieskotu tekste seku, prasidedanciu prasidedanciu
# ir pasibaigianciu ta pacia raide, kai:
#a) ta raide yra a;
str_view_all(string = testStr_ab, pattern = "a.*a")
#b) ta raide bet kuri balse
str_view_all(string = testStr_ab, pattern = "([aeiouy]).*\\1")
#c) ta raide bet kuri didzioji raide
str_view_all(string = testStr_cde, pattern = "([:upper:]).*\\1")
#d) bet kuri raide, o teksto fragmento ilgis tuei buti bent  5 simboliai
str_view_all(string = testStr_cde, pattern = "([a-zA-Z]).{3,}\\1")
#e) raide gali buti bet kokia ir ji neatzizvelgtu ir registra
str_view_all(string = testStr_cde, )