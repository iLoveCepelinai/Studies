library(ggplot2)
library(dplyr)
library(pROC)
library(caret)
library(MASS)
library(ROCR)
library(car)
library(QuantPsyc)

Sys.setlocale("LC_ALL","Lithuanian")

setwd("C:/Users/37065/Desktop/Studijos VU/6 semestras/Regresine/Tiriamasis projektas/")
data <- read.csv("HRDataset.csv")

# testine ir mokymosi aibe
set.seed(111)
samplesize = 0.80 * nrow(data)
index = sample(seq_len(nrow(data)), size = samplesize)
data_train <- data[index,]
data_test <- data[-index,]
rownames(data_train) <- 1:nrow(data_train)
rownames(data_test) <- 1:nrow(data_test)
table(data_train$Position)
tapply(data$Salary, data$Position, summary) 
summary(data$Position)

# pasiziuriu kiek darbuotoju atleido ir kiek liko dirbti
table(data['Termd'])

# pirmas modelis su visom kovariantem tik be FromDIversityFairID, nesukonverguoja
modelis1 <- glm(formula=Termd~Salary+Position+State+Sex+MaritalDesc+CitizenDesc+HispanicLatino+RaceDesc+TermReason+Department+RecruitmentSource+PerformanceScore+EngagementSurvey+EmpSatisfaction+SpecialProjectsCount+DaysLateLast30+Absences+Age+dirbo_men+Position_merged, data=data_train, family=binomial("logit"))

# ismetame kovariantes, kurios gali buti nelabai reiksmingos
modelis1 <- glm(formula=Termd~Salary+Position+State+Sex+MaritalDesc+CitizenDesc+HispanicLatino+RaceDesc+PerformanceScore+EngagementSurvey+EmpSatisfaction+SpecialProjectsCount+DaysLateLast30+Absences+Age+dirbo_men+Position_merged, data=data_train, family=binomial("logit"))
summary(modelis1)

# gaunu, kad Position_merged stulpeliui neapskaiciuoja koeficientu, dispersijos, p-reikemes, ismetu ji

#modelis2 <- glm(formula=Termd~Salary+Position+State+Sex+MaritalDesc+CitizenDesc+HispanicLatino+RaceDesc+PerformanceScore+EngagementSurvey+EmpSatisfaction+SpecialProjectsCount+DaysLateLast30+Absences+Age+dirbo_men, data=data_train, family=binomial("logit"))
#summary(modelis2)

# stulpelis Position nereiksmingas
#modelis3 <- glm(formula=Termd~Salary+State+Sex+MaritalDesc+CitizenDesc+HispanicLatino+RaceDesc+PerformanceScore+EngagementSurvey+EmpSatisfaction+SpecialProjectsCount+DaysLateLast30+Absences+Age+dirbo_men, data=data_train, family=binomial("logit"))
#summary(modelis3)

# marital status irgi nereiksmingas
#modelis4 <- glm(formula=Termd~Salary+State+Sex+CitizenDesc+HispanicLatino+RaceDesc+PerformanceScore+EngagementSurvey+EmpSatisfaction+SpecialProjectsCount+DaysLateLast30+Absences+Age+dirbo_men, data=data_train, family=binomial("logit"))
#summary(modelis4)

# rase irgi nereiksminga
modelis5 <- glm(formula=Termd~Salary+Sex+CitizenDesc+HispanicLatino+PerformanceScore+EngagementSurvey+EmpSatisfaction+SpecialProjectsCount+DaysLateLast30+Absences+Age+dirbo_men, data=data_train, family=binomial("logit"))
summary(modelis5)

#Kuko matas
cooksd <- cooks.distance(modelis5)
plot(cooksd, pch="*", cex=2, main="Kuko matas")
# yra ryski viena isskirtis, nors ji nevirsija 1, vis tiek ja pasalinu
data_train$kuko_matas <- cooksd

# Pearsono liekanos
pearson_residuals <- glm.diag(modelis5)$rp
plot(pearson_residuals, pch="*", cex=2, main="Pearson'o liekanos")
data_train$pearson <- pearson_residuals

# abiejuose grafikuose matesi po viena isskirti, ismetame jas
isskirtys <- data_train[data_train$kuko_matas > 0.4 | data_train$pearson > 4 | data_train < -4,]
data_train <- data_train[data_train$kuko_matas < 0.4 & data_train$pearson < 4 & data_train$pearson > -4,]

plot(data_train$kuko_matas, pch="*", cex=2, main="Kuko matas")
plot(data_train$pearson, pch="*", cex=2, main="Pearson'o liekanos")
# dabar isskirciu neliko

# tikrinu multikolinearuma
vif(modelis5)
# yra multikolinearumas tarp PerformanceScore ir DaysLateLast30 kovarianciu
# sudaryti du modeliai, itraukiant po viena is situ dvieju kovarianciu, o kitas paliekant

model1 <- glm(formula=Termd~Salary+Sex+CitizenDesc+HispanicLatino+PerformanceScore+EngagementSurvey+EmpSatisfaction+SpecialProjectsCount+Absences+Age+dirbo_men, data=data_train, family=binomial("logit"))
summary(model1)

model2 <- glm(formula=Termd~Salary+Sex+CitizenDesc+HispanicLatino+EngagementSurvey+EmpSatisfaction+SpecialProjectsCount+DaysLateLast30+Absences+Age+dirbo_men, data=data_train, family=binomial("logit"))
summary(model2)
# pagal AIC geresnis modelis su kovariante PerformanceScore

# kadangi net ir geresniame modelyje yra nereiksmingu kovarianciu, praleidziu funkcija stepAIC
stepAIC(model1, direction="both")

final_model <- glm(formula=Termd~CitizenDesc+PerformanceScore+EngagementSurvey+SpecialProjectsCount+Absences+dirbo_men, family=binomial("logit"), data=data_train)
summary(final_model)
confint(final_model)

# modeliai su skirtingomis saveikomis yra prastesni uz final_model
sav <- glm(formula=Termd~CitizenDesc+PerformanceScore+EngagementSurvey+PerformanceScore*SpecialProjectsCount+PerformanceScore+SpecialProjectsCount+Absences+dirbo_men, family=binomial("logit"), data=data_train)
summary(sav)

# Klasifikavimo tikslumas

rocobj <- roc(data_train$Termd, final_model$fitted.values)
c <- coords(rocobj, "best", best.method = "youden")
c$threshold

lr_prediction_test <- predict(final_model, newdata=data_test, type = "response")
classDF <- data.frame(response = data_test$Termd,
                      predicted = round(lr_prediction_test, 0))

ClassLog(final_model, data_train$Termd, cut=c$threshold)

### Klasifikavimo lentele
(class_table_logit_test <- xtabs(~ predicted + response, data = classDF))
round(prop.table(class_table_logit_test, 1),3)

# ROC kreive
pred <- predict(final_model, data_test, type = "response")
ROC_kreive <- roc(data_test$Termd, pred)
plot(ROC_kreive, col="blue", main="ROC kreive", print.auc=TRUE)
# geras rezultatas, AUC=0.992

# koeficientai
round(exp(coef(final_model)),4)
round(exp(confint(final_model)),4)

summary(final_model)
