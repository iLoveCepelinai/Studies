library(ggstatsplot)
library(boot)
library(MASS)
library(QuantPsyc)
library(pROC)
library(AER)
library(ROCR)

#  Duomenų nuskaitymas ir filtravimas
titanic_all <- read.csv("titanic_data.csv")
titanic <- titanic_all[-c(1,4,9, 11)]


# Patikriname praleistas reiksmes
sum(is.na(titanic$Survived))
sum(is.na(titanic$Pclass))
sum(is.na(titanic$Sex))
sum(is.na(titanic$SibSp))
sum(is.na(titanic$Parch))
sum(is.na(titanic$Fare))
sum(titanic$Embarked == '')
sum(is.na(titanic$Age))

titanic_f <- titanic[(!is.na(titanic$Age)) & (titanic$Embarked != ''),]


# Kategorinių kintamųjų sutaikymas
titanic_f$Pclass <- factor(titanic_f$Pclass)
titanic_f$Sex <- factor(titanic_f$Sex)
titanic_f$Embarked <- factor(titanic_f$Embarked)

titanic_f$SibSp_c <- factor(ifelse(titanic_f$SibSp == 0,'0', ifelse(titanic_f$SibSp == 1,
                                    '1', ifelse(titanic_f$SibSp == 2, '2', '>2'))))

titanic_f$Parch_c <- factor(ifelse(titanic_f$Parch == 0,'0',
                                    ifelse(titanic_f$Parch == 1, '1', 
                                    ifelse(titanic_f$Parch == 2, '2', '>2'))))

# Neisgyvenusiu ir isgyvenusiu skaicius grupese
table(titanic_f$Survived)


# Dalinimas į train ir test
set.seed(666)
samplesize = 0.80 * nrow(titanic_f)

index = sample(seq_len(nrow(titanic_f)), size =
                 samplesize)

titanic_train <- titanic_f[index,]
titanic_test <- titanic_f[-index,]

rownames(titanic_train) <- 1:nrow(titanic_train)
table(titanic_train$Survived)


# Kiekybinių duomenų vizualizavimas
ggbetweenstats(data = titanic_train, x = "Survived", y = "Age",
               plot.type = "box", mean.plotting=FALSE, results.subtitle=FALSE,
               outlier.tagging = TRUE, ylab = "Amžius", xlab = "Išgyvenamumas",
               title = "Amžiaus pasiskirstymas pagal keleivių išgyvenamumą")

ggbetweenstats(data = titanic_train, x = "Survived", y = "Fare",
               plot.type = "box", mean.plotting=FALSE, results.subtitle=FALSE,
               outlier.tagging = TRUE, ylab = "Bilieto kaina", xlab = "Išgyvenamumas",
               title = "Bilieto kainos pasiskirstymas pagal keleivių išgyvenamumą")


# Kategornių duomenų pasiskirstymas
(Pclass_freq <- xtabs(~ Pclass  + Survived, data = titanic_train))
prop.table(Pclass_freq, 1)

(Sex_freq <- xtabs(~ Sex  + Survived, data = titanic_train))
prop.table(Sex_freq, 1)

(Embarked_freq <- xtabs(~ Embarked  + Survived, data = titanic_train))
prop.table(Embarked_freq, 1)

(SibSc_freq <- xtabs(~ SibSp_c  + Survived, data = titanic_train))
prop.table(SibSc_freq, 1)

(Parch_freq <- xtabs(~ Parch_c  + Survived, data = titanic_train))
prop.table(Parch_freq, 1)


############################################
# Logit modelio taikymas (visos kovariantės)
############################################
titanic_logit <- glm(formula = Survived ~ Pclass + Sex + Age +
                       Fare + Embarked + SibSp_c + Parch_c,
    family = binomial(logit), data = titanic_train)

summary(titanic_logit)

cooksd <- cooks.distance(titanic_logit)
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks
distance")
outliers <- cooksd[cooksd>1]
outliers

p_residuals <- glm.diag(titanic_logit)$rp
plot(p_residuals, pch="*", cex=2, main="Pearson
residuals")
outliers2 <- p_residuals[abs(p_residuals)>3]
outliers2

titanic_train2 <- titanic_train[-420,]


# Logit modelio taikymas (visos kovariantės) be išskirties
titanic_logit2 <- glm(formula = Survived ~ Pclass + Sex + Age +
                       Fare + Embarked + SibSp_c + Parch_c,
                     family = binomial(logit), data = titanic_train2)

summary(titanic_logit2)

cooksd <- cooks.distance(titanic_logit2)
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks
distance")
outliers <- cooksd[cooksd>1]
outliers

p_residuals <- glm.diag(titanic_logit2)$rp
plot(p_residuals, pch="*", cex=2, main="Pearson
residuals")
outliers2 <- p_residuals[abs(p_residuals)>3]
outliers2


# Pažingsninė regresija logit modeliui
stepAIC(titanic_logit2, direction = "both")


# Logit modelio taikymas (reiksmingos kovariantės)
titanic_logit3 <- glm(formula = Survived ~ Pclass + Sex + Age +
                        SibSp_c,
                      family = binomial(logit), data = titanic_train2)

summary(titanic_logit3)
coef(titanic_logit3)
exp(coef(titanic_logit3))
exp(confint.default(titanic_logit3, level=0.95))


#---------------------------------------------------------------------
# Rezultatai
#---------------------------------------------------------------------

# Train duomenys

### Klasifikavimo lentele
(class_table_logit_train <- ClassLog(titanic_logit3, titanic_train2$Survived))

lr_prediction <- predict(titanic_logit3, titanic_train2, type = "response")

### ROC kreive
ROC_lr <- roc(titanic_train2$Survived, lr_prediction)
plot(ROC_lr, col = "blue", main = "Logit modelio ROC kreivė (mokymo duomenys)",
     xlab = "Specifiškumas", ylab = "Jautrumas")

### Youdeno indekso tikrinimas
rocobj <- roc(titanic_train2$Survived, titanic_logit3 $fitted.values)
coords(rocobj, "best", best.method="youden")



# Test duomenys
lr_prediction_test <- predict(titanic_logit3, newdata=titanic_test, type = "response")
classDF <- data.frame(response = titanic_test$Survived,
                      predicted = round(lr_prediction_test, 0))

### Klasifikavimo lentele
(class_table_logit_test <- xtabs(~ predicted + response, data = classDF))
round(prop.table(class_table_logit_test, 1),3)

### ROC kreive
ROC_lr <- roc(titanic_test$Survived, lr_prediction_test)
plot(ROC_lr, col = "blue", main = "Logit modelio ROC kreivė (testavimo duomenys)",
     xlab = "Specifiškumas", ylab = "Jautrumas")




#############################
# Probit modelio taikymas
#############################
titanic_probit <- glm(Survived ~ Pclass + Sex + Age +
                        Fare + Embarked + SibSp_c + Parch_c,
                      data = titanic_train, family =
                      binomial(link = "probit"))
summary(titanic_probit)

cooksd <- cooks.distance(titanic_probit)
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks
distance")
outliers <- cooksd[cooksd>1]
outliers

p_residuals <- glm.diag(titanic_probit)$rp
plot(p_residuals, pch="*", cex=2, main="Pearson
residuals")
outliers2 <- p_residuals[abs(p_residuals)>3]
outliers2

titanic_train2 <- titanic_train[-420,]


# Probit modelio taikymas (visos kovariantės) be išskirties
titanic_probit2 <- glm(formula = Survived ~ Pclass + Sex + Age +
                        Fare + Embarked + SibSp_c + Parch_c,
                       family = binomial(link = "probit"), data = titanic_train2)

summary(titanic_probit2)

cooksd <- cooks.distance(titanic_probit2)
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks
distance")
outliers <- cooksd[cooksd>1]
outliers

p_residuals <- glm.diag(titanic_probit2)$rp
plot(p_residuals, pch="*", cex=2, main="Pearson
residuals")
outliers2 <- p_residuals[abs(p_residuals)>3]
outliers2


# Pažingsninė probit modeliui
stepAIC(titanic_probit2)

# Probit modelio taikymas (reikšmingos kovariantės) be išskirties
titanic_probit3 <- glm(formula = Survived ~ Pclass + Sex + Age +
                         SibSp_c,
                       family = binomial(link = "probit"), data = titanic_train2)

summary(titanic_probit3)

aver_marg_effects1 <- mean(dnorm(predict(titanic_probit3,
                                         type = "link")))
aver_marg_effects1 * coef(titanic_probit3)

#-------------------------------------------------
# Rezultatai
#------------------------------------------------

#Train duomenys
(class_table_probit_train <- ClassLog(titanic_probit3, titanic_train2$Survived))

### ROC kreive
lr_prediction_test <- predict(titanic_probit3, newdata=titanic_train2, type = "response")

ROC_lr <- roc(titanic_train2$Survived, lr_prediction_test)
plot(ROC_lr, col = "blue", main = "Probit modelio ROC kreivė (mokymo duomenys)",
     xlab = "Specifiškumas", ylab = "Jautrumas")

### Youdeno indeksas
rocobj <- roc(titanic_train2$Survived, titanic_probit3$fitted.values)
coords(rocobj, "best", best.method="youden")


# Test duomenys
lr_prediction_test <- predict(titanic_probit3, newdata=titanic_test, type = "response")

### ROC kreive
ROC_lr <- roc(titanic_test$Survived, lr_prediction_test)
plot(ROC_lr, col = "blue", main = "Probit modelio ROC kreivė (testavimo duomenys)",
     xlab = "Specifiškumas", ylab = "Jautrumas")

### Klasifikavimo lentele
classDF <- data.frame(response = titanic_test$Survived,
                      predicted = round(lr_prediction_test, 0))

(class_table_probit_test <- xtabs(~ predicted + response, data = classDF))
round(prop.table(class_table_probit_test, 1),3)



###############################
# Modelių palyginimas
###############################
summary(titanic_logit3)
summary(titanic_probit3)


# Klasifikavimo matricos mokymosi duomenu
class_table_logit_train
class_table_probit_train


# Klasifikavimo matricos testiniu duomenu
class_table_logit_test
class_table_probit_test

prop.table(class_table_logit_test, 1)
prop.table(class_table_probit_test, 1)

