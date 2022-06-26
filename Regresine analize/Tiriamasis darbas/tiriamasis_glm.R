# Tiriamsis darbas
# Iveta, Jekaterina, Salvija, Sandra, Matas

####### GAMMA IR ATV. GAUSO REGRESIJA #######

# Duomenys jau sutvarkyti kitame faile
data <- read.csv("HRDataset.csv")

library(statmod)
library(dplyr)
library(MASS) #stepAIC
library(car) #vif
library(Metrics) #RMSE ir MAE
#######################################
# Dalinimas i mokymo ir testavimo aibes
#######################################
samplesize = 0.80 * nrow(data)

set.seed(111)
index = sample(seq_len(nrow(data)), size = samplesize)

data_train <- data[index,]
data_test <- data[-index,]

rownames(data_train) <- 1:nrow(data_train)
rownames(data_test) <- 1:nrow(data_test)

data_train[c(1,3:14,16)] <- lapply(data_train[c(1,3:14,16)], factor)
data_test[c(1,3:14,16)] <- lapply(data_test[c(1,3:14,16)], factor)
#######################################
# Trumpa svarbi info ir funkcijos
#######################################
hist(data_train$Salary)

# GRAFIKAI MODELIO PRIELAIDOMS
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

## QQ PLOT OF RESIDUALS (atskirai)
plots_qq <- function(modelis){
  qqnorm(qr1 <- qresid(modelis), main = "Kvantilių grafikas", xlab = "Teoriniai kvantiliai", 
         ylab = "Imties kvantiliai");
  qqline(qr1)
}


#######################################
# GAMMA REGRESIJOS MODELIS
#######################################
# neimame TermReason (nesivertina, per mazai irasu grupese)
gamma.log <- glm(Salary ~ Termd + Sex + MaritalDesc + CitizenDesc +
                   HispanicLatino + RaceDesc + Department + RecruitmentSource +
                   PerformanceScore + EngagementSurvey + EmpSatisfaction +
                   EmpSatisfaction + SpecialProjectsCount + DaysLateLast30 +
                   Absences + Age + dirbo_men + Position_merged +
                   SpecialProjectsCount:PerformanceScore,
                 family=Gamma(link="log"), data=data_train)
summary(gamma.log) # AIC 5233.8
plots_qq(gamma.log)

gamma.inv <- update(gamma.log, family=Gamma(link="inverse"))
summary(gamma.inv) # AIC = 5242.1
plots_qq(gamma.inv)

gamma.id <- update(gamma.log, family=Gamma(link="identity"))
summary(gamma.id) # AIC = 5235.9
plots_qq(gamma.id)

shapiro.test(qresid(gamma.log))

# geriausia log jubngties funkcija, toliau naudosime ja

plots(gamma.log)
# Yra isskirtys pagal standartizuotas liekanas ir kuka

######### Isskirciu salinimas #########
find_outliers <- function(model){
  cooksd <- cooks.distance(model)
  plot(cooksd, pch="*", cex=2, main="Liekanos pagal Kuko matą", ylab="", xlab="")
  outliers_cooksd <- cooksd[cooksd>1]
  pearson_residuals <- rstudent(model)
  plot(pearson_residuals, pch="*", cex=2, main="Stjudentizuotos liekanos", ylab="", xlab="")
  outliers_pearson <- pearson_residuals[abs(pearson_residuals)> 3]
  return (list(cooksd = outliers_cooksd, pearson = outliers_pearson))
}

find_outliers(gamma.log)

data_train2 <- data_train[-c(30,152),]
# 30:isskirtinai maza alga kaip network engineer;
# 152:isskirtinai maza alga kaip director.
rownames(data_train2) <- 1:nrow(data_train2)

gamma.log2 <- glm(Salary ~ Termd + Sex + MaritalDesc + CitizenDesc +
                    HispanicLatino + RaceDesc + Department + RecruitmentSource +
                    PerformanceScore + EngagementSurvey + EmpSatisfaction +
                    EmpSatisfaction + SpecialProjectsCount + DaysLateLast30 +
                    Absences + Age + dirbo_men + Position_merged +
                    SpecialProjectsCount:PerformanceScore,
                  family=Gamma(link="log"), data=data_train2)
summary(gamma.log2)# 5166.1
plots(gamma.log2)
shapiro.test(qresid(gamma.log2))

find_outliers(gamma.log2)

data_train3 <- data_train2[-126,] # kaip IT support labai maza alga
rownames(data_train3) <- 1:nrow(data_train3)

gamma.log3 <- glm(Salary ~ Termd + Sex + MaritalDesc + CitizenDesc +
                    HispanicLatino + RaceDesc + Department + RecruitmentSource +
                    PerformanceScore + EngagementSurvey + EmpSatisfaction +
                    EmpSatisfaction + SpecialProjectsCount + DaysLateLast30 +
                    Absences + Age + dirbo_men + Position_merged +
                    SpecialProjectsCount:PerformanceScore,
                  family=Gamma(link="log"), data=data_train3)
summary(gamma.log3) # AIC 5134.7
plots(gamma.log3)
shapiro.test(qresid(gamma.log3))
find_outliers(gamma.log3)

# Labai geras kvantiliu grafikas, susitvarke nukrypimai galuose

######### Reiksmingu kovarianciu atrinkimas #########
stepAIC(gamma.log3, direction = "both") # AIC 5101

gamma.log4 <- glm(formula = Salary ~ CitizenDesc + Department + PerformanceScore+
                    SpecialProjectsCount + Absences + Position_merged +
                    PerformanceScore:SpecialProjectsCount, 
                  family = Gamma(link = "log"), data = data_train3)
summary(gamma.log4) # AIC 5102.3


######### Multikolinearumo tikrinimas #########
vif(gamma.log4) # naturalu, kad yra multikolinearumas, nes naudojame saveika
confint(gamma.log4, level=0.95)
# Pasikliovimo intervalai gana nedideli, tai multikolineariu kovarianciu 
# neismesime.


######### Modelio interpretacija #########
round(exp(coef(gamma.log4)),4)




#######################################
# ATVIRKSTINIO GAUSO REGRESIJOS MODELIS
#######################################
# neimame TermReason (nesivertina, per mazai irasu grupese)
invg.log <- glm(Salary ~ Termd + Sex + MaritalDesc + CitizenDesc +
                   HispanicLatino + RaceDesc + Department + RecruitmentSource +
                   PerformanceScore + EngagementSurvey + EmpSatisfaction +
                   EmpSatisfaction + SpecialProjectsCount + DaysLateLast30 +
                   Absences + Age + dirbo_men + Position_merged +
                   SpecialProjectsCount:PerformanceScore,
                 family=inverse.gaussian(link="log"), data=data_train)
summary(invg.log) # AIC 5212.9
plots_qq(invg.log)

invg.inv <- update(invg.log, family=inverse.gaussian(link="inverse"))
summary(invg.inv) # AIC = 5219.3
plots_qq(invg.inv)

# Nesukonverguoja
invg.atvkv <- update(invg.log, family=inverse.gaussian(link="1/mu^2"))
summary(invg.atvkv) # AIC = 5247.3
plots_qq(invg.atvkv)

invg.id <- update(invg.log, family=inverse.gaussian(link="identity"))
summary(invg.id) # AIC = 5216.5
plots_qq(invg.id)

shapiro.test(qresid(invg.log))

# geriausia log jubngties funkcija, toliau naudosime ja

plots(invg.log)
# Yra isskirtys pagal standartizuotas liekanas ir kuka

find_outliers(invg.log)

data_train02 <- data_train[-c(30,127,152),]
# 30:isskirtinai maza alga kaip network engineer;
# 127:isskirtinai maza alga kaip IT support
# 152:isskirtinai maza alga kaip director.
rownames(data_train02) <- 1:nrow(data_train02)

invg.log2 <- glm(Salary ~ Termd + Sex + MaritalDesc + CitizenDesc +
                    HispanicLatino + RaceDesc + Department + RecruitmentSource +
                    PerformanceScore + EngagementSurvey + EmpSatisfaction +
                    EmpSatisfaction + SpecialProjectsCount + DaysLateLast30 +
                    Absences + Age + dirbo_men + Position_merged +
                    SpecialProjectsCount:PerformanceScore,
                  family=inverse.gaussian(link="log"), data=data_train02)
summary(invg.log2)# 5126.3
plots(invg.log2)
shapiro.test(qresid(invg.log2))

find_outliers(invg.log2)

# Nors AIC geriau nei gamma, kvantiliu grafikas atrodo ne taip gerai
plots_qq(gamma.log3)
plots_qq(invg.log2)

######### Reiksmingu kovarianciu atrinkimas #########
stepAIC(invg.log2, direction = "both") # AIC 5095

invg.log3 <- glm(formula = Salary ~ CitizenDesc + Department + PerformanceScore+
                    SpecialProjectsCount + Absences + Position_merged +
                    PerformanceScore:SpecialProjectsCount, 
                  family = inverse.gaussian(link = "log"), data = data_train02)
summary(invg.log3) # AIC 5094.9

######### Multikolinearumo tikrinimas #########
vif(invg.log3) # naturalu, kad yra multikolinearumas, nes naudojame saveika
round(exp(confint(invg.log3, level=0.95)),4)
# Pasikliovimo intervalai gana nedideli, tai multikolineariu kovarianciu 
# neismesime.


######### Modelio interpretacija #########
round(exp(coef(invg.log3)),4)

# Kodel kai absences daugeja, dideja atlyginimas?
library(ggplot2)
ggplot(data=data_train02, aes(x=Position_merged,y= Absences)) + geom_boxplot()
plot(data_train02$Absences, data_train02$Salary)
# Stai ir atsakeme i savo klausima :O




#######################################
# MODELIU PALYGINIMAS
#######################################
AIC(gamma.log4)
AIC(invg.log3)


#### Testiniai duomenys ####
pred_gamma <- predict(gamma.log4, type = "response", newdata = data_test)
pred_invg <- predict(invg.log3, type = "response", newdata = data_test)

rmse(data_test$Salary, pred_gamma)
rmse(data_test$Salary, pred_invg)

mae(data_test$Salary, pred_gamma)
mae(data_test$Salary, pred_invg)

# Pagal testinius duomenis geriau veikia gamma modelis