library(readr)
library(tidyverse)
library(statmod)

########################################################################################
#vynas
########################################################################################

wine <- read_csv("WineQT.csv")
head(wine)

# Ismetamas id
wine <- wine[,-13]

# Kintamojo alcohol pasiskirstymas
hist(wine$alcohol)

#####################################################################################
# Pradinė analizė
#####################################################################################
plot(alcohol ~ `fixed acidity`,
     xlab="Vyno rūgštingumas", ylab="Stiprumas", data=wine)

plot(alcohol ~ `volatile acidity`,
     xlab="Lakusis rūgštingumas", ylab="Stiprumas", data=wine)

plot(alcohol ~ `citric acid`,
     xlab="Citrinos rūgštis", ylab="Stiprumas", data=wine)

plot(alcohol ~ `residual sugar`,
     xlab="Liekamasis cukrus", ylab="Stiprumas", data=wine)

plot(alcohol ~ `chlorides`,
     xlab="Druskos kiekis", ylab="Stiprumas", data=wine)

plot(alcohol ~ `free sulfur dioxide`,
     xlab="Laisvasis sieros dioksidas", ylab="Stiprumas", data=wine)

plot(alcohol ~ `total sulfur dioxide`,
     xlab="Visas sieros dioksidas", ylab="Stiprumas", data=wine)

plot(alcohol ~ `density`,
     xlab="Tankis", ylab="Stiprumas", data=wine)

plot(alcohol ~ `pH`,
     xlab="pH", ylab="Stiprumas", data=wine)

plot(alcohol ~ `sulphates`,
     xlab="Sulfatai", ylab="Stiprumas", data=wine)

boxplot(alcohol ~ `quality`,
        xlab="Kokybė", ylab="Stiprumas", data=wine)


########################################################################################
# Dalinimas į mokymo ir testavimo aibes
########################################################################################
set.seed(419)
wine_f <- wine

samplesize = 0.80 * nrow(wine_f)

index = sample(seq_len(nrow(wine_f)), size =
                 samplesize)

wine <- wine_f[index,]
wine_test <- wine_f[-index,]

rownames(wine) <- 1:nrow(wine)

hist(wine$alcohol)
########################################################################################
# Liekanų analizės grafikų funkcija
########################################################################################

plots <- function(modelis){
  
  ## STDIZD RESIDUALS vs FITTED VALUES on constant-info scale
  plot(rstandard(modelis) ~ fitted(modelis),
       xlab="Įverčiai", ylab="Standartizuotos liekanos")
  
  ## LINEAR PREDICTOR
  eta.inv <- modelis$linear.predictor
  plot(resid(modelis, type="working") + eta.inv ~ eta.inv,
       ylab="Liekamosios paklaidos", xlab="Tiesinis prediktorius, eta")
  
  ## QQ PLOT OF RESIDUALS
  qqnorm(qr1 <- qresid(modelis), main = "Kvantilių grafikas", xlab = "Teoriniai kvantiliai", 
         ylab = "Imties kvantiliai");
  qqline(qr1)
  
  ## COOK'S DISTANCE
  plot( cooks.distance(modelis), xlab = "Indeksas", ylab="Kuko atstumas", type="h")
  
}


plots_qq <- function(modelis){
  ## QQ PLOT OF RESIDUALS
  qqnorm(qr1 <- qresid(modelis), main = "Kvantilių grafikas", xlab = "Teoriniai kvantiliai", 
         ylab = "Imties kvantiliai");
  qqline(qr1)
}
########################################################################################
# GAMMA link funkcijos
########################################################################################

wine.gamma <- glm(alcohol ~ ., family=Gamma(link="log"), data=wine)
summary(wine.gamma) # AIC 1605.3
plots_qq(wine.gamma)

wine.gamma.inv <- update(wine.gamma, family=Gamma(link="inverse") )
summary(wine.gamma.inv) # AIC = 1589.1
plots_qq(wine.gamma.inv)

wine.gamma.id <- update(wine.gamma, family=Gamma(link="identity") )
summary(wine.gamma.id) # AIC = 1635.4

# Naudojam link = "inverse", nes mažiausias AIC

#######################################################################################
# GAMMA 1
#######################################################################################

plots(wine.gamma.inv)

#######################################################################################
# GAMMA 2
#######################################################################################

# Kartojame pašalinus išskirtis
(indeksai <- as.numeric(names(rstandard(wine.gamma.inv)[abs(rstandard(wine.gamma.inv)) > 3])))
wine1 <- wine[-indeksai,]

wine.gamma2 <- glm(alcohol ~ ., family=Gamma(link="inverse"), data=wine1)
summary(wine.gamma2)

plots(wine.gamma2)

#######################################################################################
# GAMMA 3
#######################################################################################

# Bandom siaurinti Y kitimo sritį
hist(wine$alcohol)

wine2 <- wine1[wine1$alcohol < 13,]
hist(wine2$alcohol)

wine.gamma3 <- glm(alcohol ~ ., family=Gamma(link="inverse"), data=wine2)
summary(wine.gamma3)

plots(wine.gamma3)
# Nepadėjo

#######################################################################################
# GAMMA 4
#######################################################################################

# Tikriname multikolinearumą
library(car)
vif(wine.gamma2)
#################

# Patikrinkime, ar multikolinearumas issispres ismetus nereiksminga kovariante
wine2.1 <- wine1[,-6]
wine.gamma00 <- glm(alcohol ~ ., family=Gamma(link="inverse"), data=wine2.1)
summary(wine.gamma00)

# Ar issisprende multikolinearumas
vif(wine.gamma00)
# Ne

# Kadangi gana sunku nustatyti, su kuo koreliuoja fixed acidity, tikriname koreliacijų matricą
library(corrplot)

m <- cor(wine2.1)
corrplot(m)
# koreliuoja su ph, density ir citric acid panasiai (apie +-0.68)


wine3 <- wine2.1[,-1] # ismetus fixed acidity
wine4 <- wine2.1[,-3] # citric acid
wine5 <- wine2.1[,-8] # pH
wine6 <- wine2.1[,-7] # density


wine.gamma01 <-  glm(alcohol ~ ., family=Gamma(link="inverse"), data=wine3)
wine.gamma02 <-  glm(alcohol ~ ., family=Gamma(link="inverse"), data=wine4)
wine.gamma03 <-  glm(alcohol ~ ., family=Gamma(link="inverse"), data=wine5)
wine.gamma04 <-  glm(alcohol ~ ., family=Gamma(link="inverse"), data=wine6)

vif(wine.gamma01)
vif(wine.gamma02)
vif(wine.gamma03)
vif(wine.gamma04)

plots_qq(wine.gamma01)
plots_qq(wine.gamma02)
plots_qq(wine.gamma03)
plots_qq(wine.gamma04)

summary(wine.gamma01)
summary(wine.gamma02)
summary(wine.gamma03)
summary(wine.gamma04)

# Pasirenkame treciaji modeli (kur atmesta ph)
plots(wine.gamma03)


# Galutinis gama modelis
summary(wine.gamma03)

# Betos
wine.gamma_int <- update(wine.gamma03, family=Gamma(link="log"), data=wine5)
exp(coef(wine.gamma_int))


########################################################################################
# IG
########################################################################################

wine.ig <- glm(alcohol ~ ., family=inverse.gaussian(link="log"), data=wine)
summary(wine.ig) # AIC 1603.8

wine.ig.inv <- update(wine.ig, family=inverse.gaussian(link="inverse") )
summary(wine.ig.inv) # AIC = 1585.3

wine.ig.id <- update(wine.ig, family=inverse.gaussian(link="identity") )
summary(wine.ig.id) # AIC = 1634.5

# Naudojam link = "inverse", nes mažiausias AIC

#######################################################################################
# IG 1
#######################################################################################

plots(wine.ig.inv)


#######################################################################################
# IG 2
#######################################################################################

# Kartojame pašalinus išskirtis
(indeksai <- as.numeric(names(rstandard(wine.ig.inv)[abs(rstandard(wine.ig.inv)) > 3])))
wine1 <- wine[-indeksai,]

wine.ig2 <- glm(alcohol ~ ., family=inverse.gaussian(link="inverse"), data=wine1)
summary(wine.ig2)

plots(wine.ig2)
# Modelis tinka labiau po isskirciu salinimo

#######################################################################################
# IG 3
#######################################################################################

# Bandom siaurinti Y kitimo sritį
hist(wine1$alcohol)

wine2 <- wine1[wine1$alcohol < 13,]
hist(wine2$alcohol)

wine.ig3 <- glm(alcohol ~ ., family=inverse.gaussian(link="inverse"), data=wine2)
summary(wine.ig3)

plots(wine.ig3)
# Nepadėjo

#######################################################################################
# IG 4
#######################################################################################

# Tikriname multikolinearumą
library(car)
vif(wine.ig2)

# Patikrinkime, ar multikolinearumas issispres ismetus nereiksminga kovariante
summary(wine.ig2)
wine2.1 <- wine1[,-6]
wine.ig00 <- glm(alcohol ~ ., family=inverse.gaussian(link="inverse"), data=wine2.1)
summary(wine.ig00)

# Ar issisprende multikolinearumas
vif(wine.ig00)
# Ne

# Kadangi gana sunku nustatyti, su kuo koreliuoja fixed acidity, tikriname koreliacijų matricą
library(corrplot)

m <- cor(wine2.1)
corrplot(m)
# koreliuoja su ph, density ir citric acid panasiai (apie +-0.68)


wine3 <- wine2.1[,-1] # ismetus fixed acidity
wine4 <- wine2.1[,-3] # citric acid
wine5 <- wine2.1[,-8] # pH
wine6 <- wine2.1[,-7] # density


wine.ig01 <-  glm(alcohol ~ ., family=inverse.gaussian(link="inverse"), data=wine3)
wine.ig02 <-  glm(alcohol ~ ., family=inverse.gaussian(link="inverse"), data=wine4)
wine.ig03 <-  glm(alcohol ~ ., family=inverse.gaussian(link="inverse"), data=wine5)
wine.ig04 <-  glm(alcohol ~ ., family=inverse.gaussian(link="inverse"), data=wine6)

vif(wine.ig01)
vif(wine.ig02)
vif(wine.ig03)
vif(wine.ig04)

plots_qq(wine.ig01)
plots_qq(wine.ig02)
plots_qq(wine.ig03)
plots_qq(wine.ig04)

summary(wine.ig02) #AIC = 1451
summary(wine.ig03) #AIC = 1713.8



# Pasirenkame treciaji modeli (kur atmesta ph)
plots(wine.ig03)

# Galutinis gama modelis
summary(wine.ig03)

# Betos
wine.ig_int <- update(wine.ig03, family=inverse.gaussian(link="log"), data=wine5)
exp(coef(wine.ig_int))


########################################################################################
# Palyginam modelius
#######################################################################################
c("Gamma"=AIC(wine.gamma03), "IG"=AIC(wine.ig03))

# Pagal Pirsono statistika (bet su isimtimis)
c("Gamma" = summary(wine.gamma03)$dispersion, "IG" = summary(wine.ig03)$dispersion)
