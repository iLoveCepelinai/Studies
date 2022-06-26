#################################
#DRABUZIAI

library(readr)
productivity <- read_csv("garments_worker_productivity.csv")
prod <- productivity[,c(4,6:12,14,15)]

prod <- prod[(prod$idle_men == 0),]
prod <- prod[,-c(7,8)]
prod <- na.omit(prod)

#isskirciu salinimas
prod <- prod[(prod$incentive>0),]
prod <- prod[(prod$wip<2500),]
prod <- prod[(prod$smv<40),]
prod <- prod[(prod$over_time<25000),]
prod <- prod[(prod$no_of_workers<70),]
prod1 <- prod[(prod$actual_productivity>=prod$targeted_productivity*0.9),]


plot(prod1$targeted_productivity, prod1$actual_productivity, xlab = "Valdžios reikalavimai",
     ylab = "Produktyvumas")
plot(prod1$wip, prod1$actual_productivity, xlab = "Trūkstamos dalys produktui",
     ylab = "Produktyvumas")
plot(prod1$smv, prod1$actual_productivity, xlab = "Laikas užduočiai (min)",
     ylab = "Produktyvumas")
plot(prod1$over_time, prod1$actual_productivity, xlab = "Viršytas laikas produktui",
     ylab = "Produktyvumas")
plot(prod1$incentive, prod1$actual_productivity, xlab = "Piniginė paskata",
     ylab = "Produktyvumas")
plot(prod1$no_of_workers, prod1$actual_productivity, xlab = "Darbuotojų skaičius",
     ylab = "Produktyvumas")


library(psych)

corr.test(prod[c(2:8)])


prod1$day <- as.factor(prod1$day)
levels(prod1$day)


lm.prod <- lm(actual_productivity ~ ., data = prod1)
summary(lm.prod)
confint(lm.prod, level = 0.95)


plot(lm.prod$residuals)

plot(cooks.distance(lm.prod))

plot(rstudent(lm.prod))


#ieskome isskirciu pagal rstudent
studentas <- rstudent(lm.prod)
#studentas
indeksai <- as.numeric(names(studentas[abs(studentas) > 4]))
indeksai
#Pagal SAS truksta dar 275, 286

prod2 <- prod1[-indeksai,]
lm.prod2 <- lm(actual_productivity ~ ., data = prod2)

plot(cooks.distance(lm.prod2))

plot(rstudent(lm.prod2))

studentas2 <- rstudent(lm.prod2)

indeksai2 <- as.numeric(names(studentas2[abs(studentas2) >= 3.5]))
indeksai2
#Pavyko istrinti 275 ir 286, pagal sas liko dar 436, 274

prod3 <- prod2[-indeksai2,]
lm.prod3 <- lm(actual_productivity ~ ., data = prod3)

plot(cooks.distance(lm.prod3))

plot(rstudent(lm.prod3))

studentas3 <- rstudent(lm.prod3)
indeksai3 <- as.numeric(names(studentas3[abs(studentas3) >= 3.35]))
indeksai3

prod4 <- prod3[-indeksai3,]
lm.prod4 <- lm(actual_productivity ~ ., data = prod4)

plot(cooks.distance(lm.prod4))

par(mfrow = c(2,2))
plot(lm.prod4)
par(mfrow = c(1,1))

plot(rstudent(lm.prod4))

#Normalumo testas liekanoms
shapiro.test(lm.prod4$residuals)

summary(lm.prod4)


#Breuso - Pagano testas
library(lmtest)
bptest(lm.prod4)


library(sandwich)

#Naudojame HC0 modelio korekcija
coeftest(lm.prod4, vcov = vcovHC(lm.prod4, type = "HC0"), save = T)

summary(lm.prod4)


library(Rcmdr)
lm.prod_gal <- stepwise(mod = lm.prod4, direction = 'forward/backward', criterion = 'BIC')


library(car)
vif(lm.prod4)

summary(lm.prod_gal)

crPlots(lm.prod_gal, smooth = list(span = 0.5))

#