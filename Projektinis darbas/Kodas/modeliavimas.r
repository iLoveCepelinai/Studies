#Sys.setlocale("LC_ALL","Lithuanian")

library(data.table)
library(lubridate)
library(tidyverse)
library(MASS)#stepAIC
library(lmtest)#bptest
library(car)#vif
library(sandwich)
library(Metrics)#RMSE ir MAE


# nuskaitomi duomenys
duom<-as.data.frame(fread("galDuom.csv"))
duom$menuo <- as.factor(month(ymd(duom$data)))


library(caTools)
set.seed(67)
split = sample.split(duom$kwh, SplitRatio = 0.8)
sum(split)
sum(!split)

# Paprastu duomenu mokymo testavimo aibiu sudarymas
train = subset(duom, split == TRUE)
test = subset(duom, split == FALSE)

# Atstatome numeravima
train <- train %>% as.data.frame(row.names = 1:nrow(.))
test <- test %>% as.data.frame(row.names = 1:nrow(.))

#modelis su visom kovariantemis
names(train)
m0 <- lm(kwh ~ temp+krituliai+v_gr+slegis+d_ilg+irr+IR+menuo, data = train)
summary(m0)
# krituliai, slegis ir vejo greitis nereiksmingi. Kadangi bent vienas is
# menesiu yra reiksmingas, del to kovariante menuo yra reiksminga.

plot(m0)
plot(m0$residuals)
plot(cooks.distance(m0))
#yra kelios isskirtys, bet jos nera dideles


# Saliname isskirtis:
# didziausios isskirtys yra 12 ir 13 reiksmes
df01 <- train[-c(12, 13),] # 12c diena isskirtinai mazai energijos, 13 - daug
m00 <- lm(kwh ~ temp+krituliai+v_gr+slegis+d_ilg+irr+IR+menuo, data = df01)
summary(m00)
plot(m00)
shapiro.test(m00$residuals)
# dideliu iskirciu nebeliko, taciau normalumo salyga nera tenkinama

#library(bestNormalize)
#df04 <- df01 %>% filter(kwh > 0)
#df04$kwh_norm <- boxcox(df04$kwh)$x.t
#m02 <- lm(kwh_norm ~ temp+krituliai+v_gr+slegis+d_ilg+irr+IR+menuo, data = df04)
#plot(m02)
lambda <- 0.74
df01$kwh_bc <- (df01$kwh**lambda-1)/lambda
m01 <- lm(kwh_bc ~ d_ilg+krituliai+v_gr+slegis+irr+sqrt(IR)+temp+menuo, data = df01)
plot(m01)
summary(m01)

# Patikriname liekanu normaluma ir heteroskedatiskuma.
shapiro.test(m01$residuals)
bptest(m01)
# Liekanos pasiskirsciusios normaliai, taciau jos dera homoskedastiskos

#Naudojame HC modelio korekcija
coeftest(m01, vcov = vcovHC, save = T)

# a)
# Nereiksmingas kovariantes saliname po viena, pradedami nuo slegio:
m02 <- lm(kwh_bc ~ temp+krituliai+v_gr+d_ilg+irr+sqrt(IR)+menuo, data = df01)
coeftest(m02, vcov = vcovHC(m02))
# Saliname vejo greiti:
m03 <- lm(kwh_bc ~ temp+krituliai+d_ilg+irr+sqrt(IR)+menuo, data = df01)
coeftest(m03, vcov = vcovHC(m03))

# dabar visos kovariantes yra reiksmingos su reiksmingumo lygmeniu alfa=0.05
plot(m03)
plot(m03$residuals)
plot(cooks.distance(m03))

summary(m03)

# b)
# Atliekame pazingsnine regresija naudodami stepAIC
stepAIC(m01, direction = "both")
# Gauname, kad modelis parenkamas toks pats, kaip ir atmetus po viena kovariante (m03),
# todel toliau darba tesiame su siuo modeliu.


# Taigi, kol kas gauname, kad m03 modelio Adjusted R-squared =  0.9857.


########################################################
#       M U L T I K O L I N E A R U M A S
########################################################
# Tikriname, ar turime multikolinearumo problema:
vif(m03)

# Matome, kad temp, d_ilg, sqrt(IR) yra multikolinearios (VIF>4).
# Tikriname, kurios kovariantes maziausiai reiksmingos, kad jas pasalintume
df02<-df01
df02$sqrt_IR <- sqrt(df02$IR)

b <- summary(m03)$coef[-c(1,7:17), 1]
sx <- df02[-c(4,5,8,9,11)] %>% summarise_if(is.numeric, sd)
sy <- sd(df02$kwh_bc)
(beta <- b * sx/sy)

# Sudarome modelius pretendentus. Kadangi sqrt(IR) koeficiento zenklas tampa neigiamas 
# (ko neturetu buti, nes koreliacija teigiama), saliname sia kovariante
# Be sqrt(IR) ir d_ilg (nes labiausiai multikolinearus)
m1 <- lm(kwh_bc ~ temp+krituliai+irr+menuo, data = df01)
vif(m1)
summary(m1) # Adjusted R-squared:  0.9855

# Standartizuotos betos
b <- summary(m1)$coef[c(2:4), 1]
sx <- df02[c(2,3,7)] %>% summarise_if(is.numeric, sd)
(beta <- b * sx/sy)

# Be irr ir IR (speti pagal is anksto randamus kintamuosius (menuo multikol.))
m2 <- lm(kwh_bc ~ temp+krituliai + d_ilg, data = df01)
vif(m2)
summary(m2) # Adjusted R-squared: 0.8948

# Standartizuotos betos
b <- summary(m2)$coef[c(2:4), 1]
sx <- df02[c(2,3,6)] %>% summarise_if(is.numeric, sd)
(beta <- b * sx/sy)

# Tik kolektoriaus duomenys
m3 <- lm(kwh_bc ~ irr+sqrt(IR), data = df01)
vif(m3)
summary(m3) # Adjusted R-squared:  0.969

# Standartizuotos betos
b <- summary(m3)$coef[c(2:3), 1]
sx <- df02[c(7,12)] %>% summarise_if(is.numeric, sd)
(beta <- b * sx/sy)

# Be menesio, sqrt(IR), d_ilg (maziausiai multikolinearus modelis)
m4 <- lm(kwh_bc ~ temp+krituliai+irr, data = df01)
vif(m4)
summary(m4) # Adjusted R-squared:  0.9692

# Standartizuotos betos
b <- summary(m4)$coef[c(2:4), 1]
sx <- df02[c(2,3,7)] %>% summarise_if(is.numeric, sd)
(beta <- b * sx/sy)


cor.test(df01$krituliai, df01$kwh_bc)
# Nors krituliu koeficiento zenklas nesutampa su koreliacijos, taip gali buti, kadangi
# koreliacija tarp kwh_bc (transformuotos box-cox metrodu) yra statistiskai nereiksminga.


###################################
# Modeliu vertinimas
###################################
lambda <- 0.74
test$kwh_bc <- (test$kwh**lambda-1)/lambda

# Modelis m1 (geras R-square, didelis vif)
prediction1 <- predict(m1, newdata=test[-c(1,9)])
# Pakladios
rmse(test$kwh_bc, prediction1)
mae(test$kwh_bc, prediction1)
rmse(test$kwh_bc, prediction1)/(max(test$kwh_bc)-min(test$kwh_bc))# NRMSE
mae(test$kwh_bc, prediction1)/(max(test$kwh_bc)-min(test$kwh_bc))# NMAE


# Modelis m2 (be irr ir IR (speti pagal is anksto randamus kintamuosius (menuo multikol.)))
prediction2 <- predict(m2, newdata=test[-c(1,9)])
# Pakladios
rmse(test$kwh_bc, prediction2)
mae(test$kwh_bc, prediction2)
rmse(test$kwh_bc, prediction2)/(max(test$kwh_bc)-min(test$kwh_bc))# NRMSE
mae(test$kwh_bc, prediction2)/(max(test$kwh_bc)-min(test$kwh_bc))# NMAE


# Modelis m3 (Tik kolektoriaus duomenys)
prediction3 <- predict(m3, newdata=test[-c(1,9)])
# Pakladios
rmse(test$kwh_bc, prediction3)
mae(test$kwh_bc, prediction3)
rmse(test$kwh_bc, prediction3)/(max(test$kwh_bc)-min(test$kwh_bc))# NRMSE
mae(test$kwh_bc, prediction3)/(max(test$kwh_bc)-min(test$kwh_bc))# NMAE


# Modelis m4 (Be menesio, sqrt(IR), d_ilg (maziausiai multikolinearus modelis))
prediction4 <- predict(m4, newdata=test[-c(1,9)])
# Pakladios
rmse(test$kwh_bc, prediction4)
mae(test$kwh_bc, prediction4)
rmse(test$kwh_bc, prediction4)/(max(test$kwh_bc)-min(test$kwh_bc))# NRMSE
mae(test$kwh_bc, prediction4)/(max(test$kwh_bc)-min(test$kwh_bc))# NMAE
