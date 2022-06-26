library(readxl)
library(tidyverse)

##########################
# DUOMENU TVARKYMAS
##########################

options(scipen = 10)
movies_raw<- read_excel("2014 and 2015 CSM dataset.xlsx")

#Ismetame nereikelingus zanrus, kur filmu yra per mazai
movies <- filter(movies_raw, Genre != 4 & Genre != 6 & Genre != 7)
movies <- movies[,c(1,3,4,5)]

movies <- na.omit(movies)

#Pervadiname is skaiciu i aiskius zanru pavadinimus
movies$gen <- ifelse(movies$Genre==1, "Veiksmo", ifelse(movies$Genre==2, "Nuotykių",
                     ifelse(movies$Genre==3, "Drama", ifelse(movies$Genre==8, "Komedija",
                     ifelse(movies$Genre==9, "Biografinis", ifelse(movies$Genre==10,
                    "Kriminalinis",ifelse(movies$Genre==12, "Animacinis","Siaubo")))))))
movies$gen <- as.factor(movies$gen)


##########################
# GRAFIKAI
##########################

p1 <- ggplot(movies, aes(Gross, Ratings, colour = gen)) + geom_point(size = 3) +
  theme(legend.position="top");p1

p2 <- ggplot(movies, aes(x = gen, y = Ratings, col = gen)) +
  geom_boxplot(outlier.shape = NA) + geom_jitter(width = 0.2) +
  theme(legend.position="top");p2

p3 <- ggplot(movies, aes(x = gen, y = Gross, fill = gen)) +
  geom_boxplot(outlier.shape = NA) + geom_jitter(width = 0.2) +
  theme(legend.position="top");p3

# Sumaziname filmu uzdarbio Gross masteli (dabar bus skaiciuojama milijonais)
movies$Gross <- movies$Gross/1000000
# Matome 2 labai dideles isskirtis su Gross kovariante, jas ismetame
movies <- filter(movies, Gross < 400)


##########################
# PRIELAIDU TIKRINIMAS
##########################

# Type III SS
library(rstatix)

# Nusibrazome grafikus patikrinti, ar tikrai matome tiesinius sarysius tarp rating ir gross.
ggplot(movies, aes(Gross, Ratings, colour = gen)) + geom_point(size = 3) + 
  geom_smooth(method = "lm", aes(fill = gen), se=FALSE) + theme(legend.position="top")


# Tikriname hipotezę dėl krypties koeficientų lygybės
anova_test(data = movies, formula = Ratings ~ Gross * gen, type = 3, detailed = TRUE)
# Gauname, kad Gross::gen p reiksme = 0,625, todel H0 atmesti negalime.
# Krypties koef yra lygus.


##########################
# MODELIO PRIELAIDOS
##########################

# Tikriname, kad liekanos pasiskirsciusios pagal normaluji skirstini
shapiro.test(resid(aov(Ratings ~ gen + Gross, data = movies)))

# Naudojame Box-cox transformacija
library(MASS)

# Randame optimalia korekcija modeliui
model <- aov(Ratings ~ gen + Gross, data = movies)
bc <- boxcox(model)
(lambda <- bc$x[which.max(bc$y)])
#tikroji kitokia formule
#suapvalinkime lambda iki aiskaus 2-o laispnio

# Sukuriame nauja kintamaji rt (reitingai, pakelti kvadratu)
movies$rt <- movies$Ratings^2

# Tikriname, kad liekanos tapo pasiskirsciusios pagal normaluji skirstini
shapiro.test(resid(aov(rt ~ gen + Gross, data = movies)))


# Tikriname, kad tarp zanru grupiu dispersijos lygios
library(car)
# a) Paprastu ANOVA budu
leveneTest(rt ~ gen, data = movies)

# b) Modifikuojant reiksmes pagal pilna musu modeli (t. y. itraukiant ir Gross (kovariante))
# Tam mums reikes B reiksmes. Ja gauname is ivertinio prie gross:
# Pastaba: tam, kad rezultatai sutaptu su SAS, mes pakeiciame intercept i veiksmo filmus:
movies$gen1 <- relevel(movies$gen, ref = "Veiksmo")

# Sukuriame modeli
modelis <- lm(rt ~ Gross + gen1, data=movies)
summary(modelis)
# Gauname, kad B prie Gross yra 0.05897

# Kuriame koreguotas reitingu reiksmes pagal modeli
movies$rt_z <- movies$rt - 0.05897 * (movies$Gross - mean(movies$Gross))
# Atliekame Leveni testa koreguotam modeliui
leveneTest(rt_z ~ gen, data = movies)


##########################
# KOVARIACINE ANALIZE
##########################
anova_test(data = movies, formula = Ratings ~ Gross + gen, type = 3, detailed = TRUE)

# vidurkių palyginimai
tb <- emmeans_test(data = movies, formula = rt ~ gen, covariate = Gross, 
             p.adjust.method = "bonferroni")
filter(tb, p.adj<=0.05)


# Koreguoti reitingu vidurkiai pagal modeli 
library(emmeans)
adj_means <- emmeans_test(data = movies, formula = rt ~ gen, covariate = Gross)
get_emmeans(adj_means)

modelis <- lm(rt ~ Gross + gen1, data=movies)
summary(modelis)

# R-reiksme (determinacijos koeficientas) =  0,2567, t.y. ~26 % duomenu sklaidos galima
# nusakyti musu modeliu.
# (kokia dali duomenu sklaidos lemia skirtumai terp grupiu su skirtumais grupiu viduje)

##########################
# Dispersine analize (ANOVA)
##########################

leveneTest(rt ~ gen1, data=movies)

anova.movies <- aov(rt ~ gen1, data=movies)
#Matome, kad zanrai reiksmingai skiriasi
summary(anova.movies)

#Atliekame porinius palyginimus
tukey.rez <- TukeyHSD(anova.movies)

temp <- tukey.rez[["gen1"]][,4]
temp[temp<0.05]


