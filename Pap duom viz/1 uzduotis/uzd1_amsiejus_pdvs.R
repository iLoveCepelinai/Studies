# Matas Amšiejus
# 1 užduotis. Pirminio duomenų apdorojimo metodų taikymas


library(readr)
library(psych) # naudojama describe fjai
library(tidyverse)
library(ggpubr)
library(scales)
library(corrplot)
library(gridExtra)

################################
# 1. DUOMENU LENTELES PARUOSIMAS
################################

duom <- read_csv("Future-500-1.csv")

str(duom)
# Matome, kad revenue, expenses ir growth turetu buti skaitiniai duomenys,
# taciau nera. Taip pat id priskirkime kaip kategorini kintamaji:

duom$ID <- as.character(duom$ID)

# tvarkome revenue stulpeli
duom$Revenue <- gsub("\\$","",duom$Revenue)
duom$Revenue <- gsub(",","",duom$Revenue)
duom$Revenue <- as.numeric(duom$Revenue)

# tvarkome expenses stulpeli
duom$Expenses <- gsub(" Dollars","",duom$Expenses)
duom$Expenses <- gsub(",","",duom$Expenses)
duom$Expenses <- as.numeric(duom$Expenses)

# tvarkome expenses stulpeli
duom$Growth <- gsub("\\%","",duom$Growth)
duom$Growth <- as.numeric(duom$Growth)

################################
# 2. APRASOMOJI STATISTIKA IR DUOMENU PRIESANALIZE
################################

# a) 
# Bendra aprasomoji statistika + praleistos reiksmes:
as.table(summary(duom))

# Patogu tuom, kad graziai sudeda i lentele
apras_stat <- describe(duom[-c(1,2,3,6,7)], quant = c(0.25,0.75))
apras_stat <- round(apras_stat, 2)
apras_stat <- apras_stat[,-c(1,6,7,11,12,13)]

#write.csv(apras_stat, file = "pirma_lent.csv")

# b)
# Statistika pagal pramones sakas (nespausdina 1 ir 3 kvartiliu, kaip?)
apras_stat_pram <- describeBy(duom[-c(1,2,3,6,7)], group = duom$Industry, mat = T,
                              digits = 2, quant = c(0.25, 0.75))

apras_stat_pram <- apras_stat_pram[,-c(1,3,8,9,13,14,15)]

#write.csv(apras_stat_pram, file = "antra_lent.csv")

################################
# 3. PRALEISTU REIKSMIU TVARKYMAS
################################
# Atsargine kopija
duom_backup <- duom
#duom <- duom_backup

# Pirma isrinksime reiksmes, kuriu nera kaip uzpildyti.
# Industry
duom <- duom[!is.na(duom$Industry),]
# Inception
duom <- duom[!is.na(duom$Inception),]


# Toliau pildysime employees

#Patikrinkime, kur truksta reiksmiu:
duom[is.na(duom$Employees),]
#Truksta Retail, Health ir Financial Sector.

# a) pagal visos imties mediana
# is lenteles apras_stat matome, kad mediana 56, o vidurkis 149. Matome dideli skirtuma, 
# pabandykime paanalozuoti, kas geriau.
hist(duom$Employees)
# Turime akivaizdzia isskirti, tad arba tektu imti mediana, arba nupjautini vidurki, kuris
# yra ~81

# b) pagal mediana pramones grupese (industry)
# is lenteles apras_stat_pram matome, kad tiek vidurkiai, tiek medianos tarp kai kuriu grupiu
# gana smarkiai skiriasi. Tai sufleruoja, jog vertetu atsizvelgti i pramones tipa uzpildant
# praleistas reiksmes.

# grupuota <- duom %>% group_by(Industry) %>% summarise(mean)
temp <- duom
temp <- temp[!is.na(temp$Employees),]
temp <- temp[!c(temp$Employees>6000),]

ggerrorplot(data = temp, x = 'Industry', y = 'Employees', 
            desc_stat = "median_iqr",
            add = "mean") +
  xlab("Pramonė") + ylab("Darb. sk.") + theme_bw()

#Si karta pakeiskime praleistas reiksmes pagal medianas pramones grupese
duom$Employees = round(ifelse(is.na(duom$Employees),
                      ave(duom$Employees, duom$Industry,
                          FUN = function(x) median(x, na.rm = TRUE)),
                      duom$Employees), 0)

# Patikriname, ar nebeliko tusciu reiksmiu:
duom[is.na(duom$Employees),]


# Dabar uzpildysime praleistas State reiksmes
duom[is.na(duom$State),]

#Turime uzpildyti 2 eil NY ir 2 eil CA
duom[is.na(duom$State) & duom$City=="New York", "State"] <- "NY"
duom[is.na(duom$State) & duom$City=="San Francisco", "State"] <- "CA"


# Revenue
duom[is.na(duom$Revenue),]

#Jei imanoma, pagal formule
duom[is.na(duom$Revenue), "Revenue"] <- duom[is.na(duom$Revenue), "Profit"] + 
  duom[is.na(duom$Revenue), "Expenses"]

# Uzpildome pagal mediana pramonese. Isiminkime eilutes 8 ir 44
duom$Revenue = ifelse(is.na(duom$Revenue),
                      ave(duom$Revenue, duom$Industry,
                          FUN = function(x) median(x, na.rm = TRUE)),
                          duom$Revenue)

# Profit
duom[is.na(duom$Profit),]

duom[is.na(duom$Profit), "Profit"] <- duom[is.na(duom$Profit), "Revenue"] - 
  duom[is.na(duom$Profit), "Expenses"]

# Expenses
duom[is.na(duom$Expenses),]

duom[is.na(duom$Expenses), "Expenses"] <- duom[is.na(duom$Expenses), "Revenue"] -
  duom[is.na(duom$Expenses), "Profit"]

# Istriname reiksmes, kurias pildyti nekorektiska
duom <- duom[!is.na(duom$Expenses),]

# Uzpildome tuscias growth reiksmes
duom[is.na(duom$Growth),]

duom$Growth = ifelse(is.na(duom$Growth),
                      ave(duom$Growth, duom$Industry,
                          FUN = function(x) median(x, na.rm = TRUE)),
                      duom$Growth)

# Patikriname, ar nebeliko tusciu reiksmiu
duom[!complete.cases(duom), ]

################################
# 4. ISSKIRCIU SALINIMAS
################################

isskirtys <- function(stulp, daugikl){
  iqr <- IQR(stulp)
  Q1<-as.numeric(summary(stulp)[2])
  Q3<-as.numeric(summary(stulp)[5])
  lower_bound <- Q1 - daugikl * iqr
  upper_bound <- Q3 + daugikl * iqr
  
  
  outliers <- which(stulp < lower_bound | stulp > upper_bound)
}

# Revenue
# Salygines isskirtys
eilut <- isskirtys(duom$Revenue, 1.5)
duom[eilut,]

# Isskirtys
eilut <- isskirtys(duom$Revenue, 3)
duom[eilut,]

#boxplot(duom$Revenue)


# Expenses
# Salygines isskirtys
eilut <- isskirtys(duom$Expenses, 1.5)
duom[eilut,]

# Isskirtys
eilut <- isskirtys(duom$Expenses, 3)
duom[eilut,]

#boxplot(duom$Expenses)


# Profit
eilut <- isskirtys(duom$Profit, 1.5)
duom[eilut,]

# Isskirtys
eilut <- isskirtys(duom$Profit, 3)
duom[eilut,]

#boxplot(duom$Profit)


# Employees
# Salygines isskirtys
eilut <- isskirtys(duom$Employees, 1.5)
duom[eilut,]

# Isskirtys
eilut <- isskirtys(duom$Employees, 3)
duom[eilut,]

#duom %>% arrange(desc(Employees)) %>% head(10)

#boxplot(duom$Employees)

#duom_backup <- duom
#duom <- duom_backup

# Istriname isskirtis
duom <- duom[-c(eilut),]

duom$Outlier <- 0
# Naujos isskirtys, kurias deretu uzfiskuoti
eilut <- isskirtys(duom$Employees, 3)
duom$Outlier[c(eilut)] <- 1


ggplot(duom, aes(x=Revenue, y=Expenses, color=Employees)) + geom_point(size = 5.5, 
                                                                       alpha = 0.9) +
  scale_y_continuous(labels = comma) +
  scale_x_continuous(labels = comma) +
  labs(title = "Sklaidos diagrama", y = "Išlaidos", x = "Pajamos") +
  scale_colour_continuous("Darbuotjų sk.")

# Matome, kad imones, su daug darbuotoju yra susimaisiusios tarp kitu, t. y. jos 
# neissiskiria kitais bruozais.

# Profit
eilut <- isskirtys(duom$Profit, 3)
eilut <- isskirtys(duom$Profit, 1.5)
# Isskirciu nera

# Employees
apras_stat[2,2] #vidurkis
apras_stat[2,3] #stand nuok
apras_stat[2,4] #mediana

# -||- po iskirciu salinimo:
apras_stat_red <- round(describe(duom[-c(1,2,3,6,7)]), 2)
apras_stat_red[2,3] #vidurkis
apras_stat_red[2,4] #stand nuok
apras_stat_red[2,5] #mediana

#Revenue
apras_stat[3,2] #vidurkis
apras_stat[3,3] #dispersija
apras_stat[3,4] #mediana

# -||- po iskirciu salinimo:
apras_stat_red[3,3] #vidurkis
apras_stat_red[3,4] #dispersija
apras_stat_red[3,5] #mediana


# Nauji aprasomosios statistikos duomenys pagal pramone
apras_stat_red_pram <- describeBy(duom[-c(1,2,3,6,7)], group = duom$Industry, mat = T,
           digits = 2, quant = c(0.25, 0.75))

################################
# 5. NORMAVIMAS
################################

duom2 <- duom

# Normavimo funkcija pagal min max
min_max_func <- function(stulp) {
  mini <- min(stulp)
  maxi <- max(stulp)
  normStulp <- (stulp - mini) / (maxi - mini)
}

# Normuojame Employees
normuota <- min_max_func(duom2$Employees)
duom2$Employees <- normuota

# Normuojame Revenue
normuota <- min_max_func(duom2$Revenue)
duom2$Revenue <- normuota

# Normuojame Profit
normuota <- min_max_func(duom2$Profit)
duom2$Profit <- normuota

# Normuojame Expenses
normuota <- min_max_func(duom2$Expenses)
duom2$Expenses <- normuota

# PADARYTI GROWTH NORMAVIMA!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# !!!!!!!!!!!

# Normavimo funkcija pagal vidurki ir dispersija
norm_func <- function(stulp) {
  vid <- mean(stulp)
  stNuok <- sd(stulp)
  stulp_norm <- (stulp - vid) / stNuok
}

duom3 <- duom

# Normuojame Employees
normuota <- norm_func(duom3$Employees)
duom3$Employees <- normuota

# Normuojame Revenue
normuota <- norm_func(duom3$Revenue)
duom3$Revenue <- normuota

# Normuojame Profit
normuota <- norm_func(duom3$Profit)
duom3$Profit <- normuota

# Normuojame Expenses
normuota <- norm_func(duom3$Expenses)
duom3$Expenses <- normuota

# Taskines diagramos
#ggplot(duom, aes(x=Revenue, y=Expenses, color=Employees)) + geom_point(size = 4)
#ggplot(duom2, aes(x=Revenue, y=Expenses, color=Employees)) + geom_point(size = 4)
#ggplot(duom3, aes(x=Revenue, y=Expenses, color=Employees)) + geom_point(size = 4)


# Stulpelines diagramos
ggplot(duom, aes(x=Industry, y=Revenue, fill = Industry)) + geom_col() +
  scale_y_continuous(labels = comma) + 
  theme(legend.position="none") + 
  labs(title = "Nenormuotų duomenų stulpelinė diagrama", y = "Pajamos", x = "Pramonė") +
  scale_x_discrete(labels = c("Construction" = "Statybos", "Financial Services" = "Finansai", 
                              "Government Services" = "Valstybinės įmonės", "Health" = 
                                "Sveikata", "IT Services" = "IT", "Retail" = 
                                "Prekyba", "Software" = "Programinė įranga"))

ggplot(duom2, aes(x=Industry, y=Revenue, fill = Industry)) + geom_col() + 
  theme(legend.position="none") +
  labs(title = "Normuotų duomenų min-max metodu stulpelinė diagrama",
       y = "Pajamos", x = "Pramonė") + 
  scale_x_discrete(labels = c("Construction" = "Statybos", "Financial Services" = "Finansai", 
                              "Government Services" = "Valstybinės įmonės", "Health" = 
                                "Sveikata", "IT Services" = "IT", "Retail" = 
                                "Prekyba", "Software" = "Programinė įranga"))

ggplot(duom3, aes(x=Industry, y=Revenue, fill = Industry)) + geom_col()+ 
  theme(legend.position="none") +
  labs(title = "Normuotų duomenų vidurkio ir dispersijos metodu stulpelinė diagrama",
       y = "Pajamos", x = "Pramonė") + 
  scale_x_discrete(labels = c("Construction" = "Statybos", "Financial Services" = "Finansai", 
                              "Government Services" = "Valstybinės įmonės", "Health" = 
                                "Sveikata", "IT Services" = "IT", "Retail" = 
                                "Prekyba", "Software" = "Programinė įranga"))


################################
# 6. VIZUALIZAVIMAS
################################
# TASKINES DIAGRAMOS

ggplot(duom, aes(x=Employees, y=Profit, color=Industry)) +
  geom_point(size = 6, alpha = 0.9) +
  theme_bw() + scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = comma) + 
  labs(title = "Įmonių sklaida pagal darbuotojus ir pelną",
       y = "Pelnas", x = "Darbuotojai") +
  scale_color_discrete(name = "Pramonė", labels = c("Construction" = "Statybos",
                                 "Financial Services" = "Finansai", 
                              "Government Services" = "Valstybinės įmonės", "Health" = 
                                "Sveikata", "IT Services" = "IT", "Retail" = 
                                "Prekyba", "Software" = "Programinė įranga"))

ggplot(duom, aes(x=Revenue, y=Expenses, color=Industry)) +
  geom_point(size = 5.5, alpha = 0.8) +
  theme_bw() + scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = comma)+ 
  labs(title = "Įmonių sklaida pagal pajamas ir išlaidas",
       y = "Išlaidos", x = "Pajamos") +
  scale_color_discrete(name = "Pramonė", labels = c("Construction" = "Statybos",
                        "Financial Services" = "Finansai", 
                        "Government Services" = "Valstybinės įmonės", "Health" = 
                          "Sveikata", "IT Services" = "IT", "Retail" = 
                          "Prekyba", "Software" = "Programinė įranga"))


test <- duom
test$Darbuotojai<- test$Employees

ggplot(test, aes(x=Revenue, y=Profit, size = Darbuotojai, color=Employees)) +
  geom_point(alpha = 0.9) +
  theme_bw() + scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = comma) +
  labs(title = "Įmonių sklaida pagal pajamas ir pelną",
       y = "Pelnas", x = "Pajamos") +
  scale_colour_continuous("Darbuotojai")


# DAZNIU DIAGRAMOS
ggplot(duom, aes(x=Employees)) +
  geom_histogram(aes(fill = Industry), binwidth = 50, colour = "black", size = 0.5) +
  theme_bw() + 
  scale_fill_discrete(name = "Pramonė", labels = c("Construction" = "Statybos",
                                    "Financial Services" = "Finansai", 
                                    "Government Services" = "Valstybinės įmonės", "Health" = 
                                      "Sveikata", "IT Services" = "IT", "Retail" = 
                                      "Prekyba", "Software" = "Programinė įranga")) +
  labs(title = "Įmonių kiekis pagal darbuotojų skaičių",
       y = "Kiekis", x = "Darbuotojai")
  

ggplot(duom, aes(x=Inception)) +
  geom_bar(aes(fill = Industry), colour = "black", size = 0.5) +
  scale_x_continuous(breaks = seq(1999, 2014, by = 1)) + 
  scale_fill_discrete(name = "Pramonė", labels = c("Construction" = "Statybos",
                                            "Financial Services" = "Finansai",
                              "Government Services" = "Valstybinės įmonės", "Health" =
                                "Sveikata", "IT Services" = "IT", "Retail" =
                                "Prekyba", "Software" = "Programinė įranga")) +
  labs(title = "Įsikūrusių įmonių kiekis pagal metus",
       y = "Kiekis", x = "Metai")


#Suma
ggplot(duom, aes(x=Industry, y=Employees, fill = Industry)) +
  geom_col() + theme(legend.position="none") +
  labs(title = "Darbuotojų sk. pagal pramonės šaką (Suma)", y = "Darbuotojų sk.", x = "Pramonės šaka") +
  scale_x_discrete(labels = c("Construction" = "Statybos", "Financial Services" = "Finansai",
                              "Government Services" = "Valstybinės įmonės", "Health" =
                                "Sveikata", "IT Services" = "IT", "Retail" =
                                "Prekyba", "Software" = "Programinė įranga"))

ggplot(duom, aes(x=Industry, y=Profit, fill = Industry)) +
  geom_col() + theme(legend.position="none") + scale_y_continuous(labels = comma) +
  labs(title = "Pelnas pagal pramonės šaką (Suma)", y = "Pelnas", x = "Pramonės šaka") +
  scale_x_discrete(labels = c("Construction" = "Statybos", "Financial Services" = "Finansai",
                              "Government Services" = "Valstybinės įmonės", "Health" =
                                "Sveikata", "IT Services" = "IT", "Retail" =
                                "Prekyba", "Software" = "Programinė įranga"))


ggplot(duom, aes(x=Industry, y=Expenses, fill = Industry)) +
  geom_col() + theme(legend.position="none") + scale_y_continuous(labels = comma) +
  labs(title = "Išlaidos pagal pramonės šaką (Suma)", y = "Išlaidos", x = "Pramonės šaka") +
  scale_x_discrete(labels = c("Construction" = "Statybos", "Financial Services" = "Finansai",
                              "Government Services" = "Valstybinės įmonės", "Health" =
                                "Sveikata", "IT Services" = "IT", "Retail" =
                                "Prekyba", "Software" = "Programinė įranga"))


ggplot(duom, aes(x=Industry, fill = Industry)) +
  geom_bar() + theme(legend.position="none") +
  labs(title = "Įmonių skaičius pagal pramonės šaką (Suma)", y = "Kiekis", x = "Pramonės šaka") +
  scale_x_discrete(labels = c("Construction" = "Statybos", "Financial Services" = "Finansai",
                              "Government Services" = "Valstybinės įmonės", "Health" =
                                "Sveikata", "IT Services" = "IT", "Retail" =
                                "Prekyba", "Software" = "Programinė įranga"))


#Vidurkiai
test <- as.data.frame(aggregate(duom$Employees, list(duom$Industry), FUN=mean))
names(test) <- c("Industry", "Mean employees")
ggplot(test, aes(x=Industry, y=`Mean employees`, fill = Industry)) +
  geom_col() + theme(legend.position="none") +
  labs(title = "Vidutinis darbuotojų sk. pagal pramonės šaką", y = "Darbuotojų. sk.", x = "Pramonės šaka")+
  scale_x_discrete(labels = c("Construction" = "Statybos", "Financial Services" = "Finansai",
                              "Government Services" = "Valstybinės įmonės", "Health" =
                                "Sveikata", "IT Services" = "IT", "Retail" =
                                "Prekyba", "Software" = "Programinė įranga"))


test <- as.data.frame(aggregate(duom$Profit, list(duom$Industry), FUN=mean))
names(test) <- c("Industry", "Mean profit")
ggplot(test, aes(x=Industry, y=`Mean profit`, fill = Industry)) +
  geom_col() + theme(legend.position="none") + scale_y_continuous(labels = comma) +
  labs(title = "Vidutinis pelnas pagal pramonės šaką", y = "Pelnas", x = "Pramonės šaka") +
  scale_x_discrete(labels = c("Construction" = "Statybos", "Financial Services" = "Finansai",
                              "Government Services" = "Valstybinės įmonės", "Health" =
                                "Sveikata", "IT Services" = "IT", "Retail" =
                                "Prekyba", "Software" = "Programinė įranga"))


test <- as.data.frame(aggregate(duom$Expenses, list(duom$Industry), FUN=mean))
names(test) <- c("Industry", "Mean expenses")
ggplot(test, aes(x=Industry, y=`Mean expenses`, fill = Industry)) +
  geom_col() + theme(legend.position="none") + scale_y_continuous(labels = comma) +
  labs(title = "Vidutinės išlaidos pagal pramonės šaką", y = "Išlaidos", x = "Pramonės šaka") +
  scale_x_discrete(labels = c("Construction" = "Statybos", "Financial Services" = "Finansai",
                              "Government Services" = "Valstybinės įmonės", "Health" =
                                "Sveikata", "IT Services" = "IT", "Retail" =
                                "Prekyba", "Software" = "Programinė įranga"))



pop_valst <- duom %>% group_by(State) %>%
  summarise(Kiekis_vals = n()) %>% 
  arrange(desc(Kiekis_vals)) %>%
  head(10)


# Staciakampes diagramos
ggplot(duom, aes(x = Industry, y = Employees, fill = Industry)) + geom_boxplot() +
  theme(legend.position="none") +
  labs(title = "Darbuotojų sk. pagal pramonės šaką", y = "Darbuotojų sk.", x = "Pramonės šaka") +
  scale_x_discrete(labels = c("Construction" = "Statybos", "Financial Services" = "Finansai",
                              "Government Services" = "Valstybinės įmonės", "Health" =
                                "Sveikata", "IT Services" = "IT", "Retail" =
                                "Prekyba", "Software" = "Programinė įranga"))


ggplot(duom, aes(x = Industry, y = Profit, fill = Industry)) + geom_boxplot() +
  scale_y_continuous(labels = comma) + theme(legend.position="none") +
  labs(title = "Pelnas pagal pramonės šaką", y = "Pelnas", x = "Pramonės šaka") +
  scale_x_discrete(labels = c("Construction" = "Statybos", "Financial Services" = "Finansai",
                              "Government Services" = "Valstybinės įmonės", "Health" =
                                "Sveikata", "IT Services" = "IT", "Retail" =
                                "Prekyba", "Software" = "Programinė įranga"))


ggplot(duom, aes(x = Industry, y = Growth, fill = Industry)) + geom_boxplot() +
  scale_y_continuous(labels = comma) + theme(legend.position="none") +
  labs(title = "Augimas pagal pramonės šaką", y = "Augimas", x = "Pramonės šaka") +
  scale_x_discrete(labels = c("Construction" = "Statybos", "Financial Services" = "Finansai",
                              "Government Services" = "Valstybinės įmonės", "Health" =
                                "Sveikata", "IT Services" = "IT", "Retail" =
                                "Prekyba", "Software" = "Programinė įranga"))

################################
# 7. KORELIACIJOS
################################
m <- cor(duom[,c(4,5,8,9,10,11)])
colnames(m) <- c("Metai", "Darb. sk.", "Pajamos", "Išlaidos", "Pelnas", "Augimas")
rownames(m) <- c("Metai", "Darb. sk.", "Pajamos", "Išlaidos", "Pelnas", "Augimas")
corrplot(m, method = "color", title = "Požymių koreliacijos", mar=c(0,0,1,0))

pav <- round(m, 2)


duom %>% filter(Industry == "IT Sector") %>% summarise(sum(Employees))


