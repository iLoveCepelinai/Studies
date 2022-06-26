# 4 uzduotis
# Roland G. ir Matas A.

library(mnist)
library(dplyr)
library(ggplot2)
library(caTools)# test-train split
library(caret)# CV
library(Rtsne)
library(e1071)# SVM
library(randomForest)# RF
library(ROCR)
library(pROC)


##################################
# Duomenu tvarkymas
##################################
mnist <- download_mnist()

# Atrenkame tik 5 ir 8 skaitmenis
set.seed(67)
data <- mnist %>% filter(Label == '5' | Label == '8') %>%
  group_by(Label) %>% sample_n(size = 1000)
data$Label <- as.character(data$Label)

# Duomenu normuoti nereikia - jie visi toje pacioje skaleje


# Duomenu test train indeksu sudarymas
set.seed(67)
split = sample.split(data$Label, SplitRatio = 0.8)
sum(split) # 1600 mokymo
sum(!split) # 400 testavimo


# Paprastu duomenu mokymo testavimo aibiu sudarymas
train = subset(data, split == TRUE)
test = subset(data, split == FALSE)


# t-SNE duomenu sudarymas
set.seed(67)
tsne <- Rtsne(data, dims = 2, perplexity=30, verbose=TRUE, max_iter = 1000)
data_tsne <- as.data.frame(tsne$Y)
data_tsne$Label <- data$Label

# t-SNE duomenu dalinimas
train_tsne = subset(data_tsne, split == TRUE)
test_tsne = subset(data_tsne, split == FALSE)


##################################
# SVM
##################################

# Klasifikavimo tikslumo matai (funkcija)
tikslumo_matai <- function(cm, n){
  diag = diag(cm) # number of correctly classified instances per class 
  rowsums = apply(cm, 1, sum) # number of predictions per class
  colsums = apply(cm, 2, sum) # number of instances per class
  
  accuracy = sum(diag) / n # general accuracy of classification
  # matai skaiciuojami atskiroms klasems
  precision = diag / rowsums
  recall = diag / colsums
  f1 = 2 * precision * recall / (precision + recall)
  print("Matai atskiroms klasems")
  print(data.frame(precision, recall, f1))
  
  # matai apskaiciuoti testavimo aibei
  macroPrecision = mean(precision)
  macroRecall = mean(recall)
  macroF1 = mean(f1)
  print("Matai apskaiciuoti testavimo aibei")
  print(data.frame(macroPrecision, macroRecall, macroF1))
}

# Klasifikavimo tikslumo matai (funkcija) (parametru parinkimui)
tikslumo_matai_param <- function(cm, n){
  diag = diag(cm) # number of correctly classified instances per class 
  rowsums = apply(cm, 1, sum) # number of predictions per class
  colsums = apply(cm, 2, sum) # number of instances per class
  
  accuracy = sum(diag) / n # general accuracy of classification
  # matai skaiciuojami atskiroms klasems
  precision = diag / rowsums
  recall = diag / colsums
  f1 = 2 * precision * recall / (precision + recall)
  
  # matai apskaiciuoti testavimo aibei
  macroF1 = mean(f1)
  
  data.frame(accuracy, macroF1)
}

cost <- 1
epsilon <- 0.1
degree <- c(1,3,5) # default = 3
gamma <- c(1/784,0.001,0.1) # default = 1/dim(duomenys)<=>1/784
coef0 <- c(-0.5, 0, 0.1) # default = 0

# LINEAR
temp <- tidyr::crossing(cost, epsilon)
for(index in 1:nrow(temp)){
  classifier_temp = svm(formula = Label ~ .,
                   data = train,
                   type = 'C-classification',
                   kernel = 'linear',
                   scale = FALSE,
                   cost = temp[index,1],
                   epsilon = temp[index,2])
  y_pred_temp <- predict(classifier_temp, newdata = test[,-785])
  cm_temp <- table(y_pred_temp,t(test[, 785]))
  temp_iv <- tikslumo_matai_param(cm_temp, nrow(test[,785]))
  temp$Accuracy[index] <- temp_iv$accuracy
  temp$F1[index] <- temp_iv$macroF1
  temp
}
param_svm_linear <- temp

# POLYNOMIAL
temp <- tidyr::crossing(cost, epsilon, degree, gamma, coef0)
for(index in 1:nrow(temp)){
  classifier_temp = svm(formula = Label ~ .,
                        data = train,
                        type = 'C-classification',
                        kernel = 'polynomial',
                        scale = FALSE,
                        cost = temp[index,1],
                        epsilon = temp[index,2],
                        degree = temp[index,3],
                        gamma = temp[index,4],
                        coef0 = temp[index,5])
  y_pred_temp <- predict(classifier_temp, newdata = test[,-785])
  cm_temp <- table(y_pred_temp,t(test[, 785]))
  temp_iv <- tikslumo_matai_param(cm_temp, nrow(test[,785]))
  temp$Accuracy[index] <- temp_iv$accuracy
  temp$F1[index] <- temp_iv$macroF1
  temp
}
param_svm_poly <- temp[,c(3:7)]
write.csv(x = param_svm_poly, file = "temp.csv")

# RADIAL BASIS
temp <- tidyr::crossing(cost, epsilon, gamma)
for(index in 1:nrow(temp)){
  classifier_temp = svm(formula = Label ~ .,
                        data = train,
                        type = 'C-classification',
                        kernel = 'radial',
                        scale = FALSE,
                        cost = temp[index,1],
                        epsilon = temp[index,2],
                        gamma = temp[index,3])
  y_pred_temp <- predict(classifier_temp, newdata = test[,-785])
  cm_temp <- table(y_pred_temp,t(test[, 785]))
  temp_iv <- tikslumo_matai_param(cm_temp, nrow(test[,785]))
  temp$Accuracy[index] <- temp_iv$accuracy
  temp$F1[index] <- temp_iv$macroF1
  temp
}
param_svm_rb <- temp[,c(3:5)]
write.csv(x = param_svm_rb, file = "temp2.csv")

# SIGMOID
temp <- tidyr::crossing(cost, epsilon, gamma, coef0)
for(index in 1:nrow(temp)){
  classifier_temp = svm(formula = Label ~ .,
                        data = train,
                        type = 'C-classification',
                        kernel = 'sigmoid',
                        scale = FALSE,
                        cost = temp[index,1],
                        epsilon = temp[index,2],
                        gamma = temp[index,3],
                        coef0 = temp[index,4])
  y_pred_temp <- predict(classifier_temp, newdata = test[,-785])
  cm_temp <- table(y_pred_temp,t(test[, 785]))
  temp_iv <- tikslumo_matai_param(cm_temp, nrow(test[,785]))
  temp$Accuracy[index] <- temp_iv$accuracy
  temp$F1[index] <- temp_iv$macroF1
  temp
}
param_svm_sigmoid <- temp[,c(3:6)]
write.csv(x = param_svm_sigmoid, file = "temp3.csv")

# Geriausia su kernel polynomial su hyperparametais degree=3 (default),
# kiti parametrai itakos neturejo.

# Geriausio varianto modelis
classifier = svm(formula = Label ~ .,
                 data = train,
                 type = 'C-classification',
                 kernel = 'polynomial',
                 scale = FALSE)
  #kadangi kitos reiksmes nesvarbios, paliksim default

y_pred1 = predict(classifier, newdata = test[,-785])
# Klasifikavimo matrica
(cm1 = table(y_pred1,t(test[, 785])))
tikslumo_matai(cm1, nrow(test[,785]))

temp <- as.data.frame(train$Label)

y_pred1

par(mfrow=c(2, 2))
show_digit(test, 33)
show_digit(test, 42)
show_digit(test, 357)
show_digit(test, 372)
par(mfrow=c(1, 1))

##################################
# SVM t-SNE
##################################

# LINEAR
temp <- tidyr::crossing(cost, epsilon)
for(index in 1:nrow(temp)){
  classifier_temp = svm(formula = Label ~ .,
                        data = train_tsne,
                        type = 'C-classification',
                        kernel = 'linear',
                        scale = FALSE,
                        cost = temp[index,1],
                        epsilon = temp[index,2])
  y_pred_temp <- predict(classifier_temp, newdata = test_tsne[,-3])
  cm_temp <- table(y_pred_temp,t(test_tsne[,3]))
  temp_iv <- tikslumo_matai_param(cm_temp, length(test_tsne[,3]))
  temp$Accuracy[index] <- temp_iv$accuracy
  temp$F1[index] <- temp_iv$macroF1
  temp
}
par_tsne_svm_linear <- temp

# POLYNOMIAL
temp <- tidyr::crossing(cost, epsilon, degree, gamma, coef0)
for(index in 1:nrow(temp)){
  classifier_temp = svm(formula = Label ~ .,
                        data = train_tsne,
                        type = 'C-classification',
                        kernel = 'polynomial',
                        scale = FALSE,
                        cost = temp[index,1],
                        epsilon = temp[index,2],
                        degree = temp[index,3],
                        gamma = temp[index,4],
                        coef0 = temp[index,5])
  y_pred_temp <- predict(classifier_temp, newdata = test_tsne[,-3])
  cm_temp <- table(y_pred_temp,t(test_tsne[, 3]))
  temp_iv <- tikslumo_matai_param(cm_temp, length(test_tsne[,3]))
  temp$Accuracy[index] <- temp_iv$accuracy
  temp$F1[index] <- temp_iv$macroF1
  temp
}
par_tsne_svm_poly <- temp[,c(3:7)]
write.csv(x = par_tsne_svm_poly, file = "temp4.csv")

# RADIAL BASIS
temp <- tidyr::crossing(cost, epsilon, gamma)
for(index in 1:nrow(temp)){
  classifier_temp = svm(formula = Label ~ .,
                        data = train_tsne,
                        type = 'C-classification',
                        kernel = 'radial',
                        scale = FALSE,
                        cost = temp[index,1],
                        epsilon = temp[index,2],
                        gamma = temp[index,3])
  y_pred_temp <- predict(classifier_temp, newdata = test_tsne[,-3])
  cm_temp <- table(y_pred_temp,t(test_tsne[, 3]))
  temp_iv <- tikslumo_matai_param(cm_temp, length(test_tsne[,3]))
  temp$Accuracy[index] <- temp_iv$accuracy
  temp$F1[index] <- temp_iv$macroF1
  temp
}
par_tsne_svm_rb <- temp[,c(3:5)]
write.csv(x = par_tsne_svm_rb, file = "temp5.csv")

# SIGMOID
temp <- tidyr::crossing(cost, epsilon, gamma, coef0)
for(index in 1:nrow(temp)){
  classifier_temp = svm(formula = Label ~ .,
                        data = train_tsne,
                        type = 'C-classification',
                        kernel = 'sigmoid',
                        scale = FALSE,
                        cost = temp[index,1],
                        epsilon = temp[index,2],
                        gamma = temp[index,3],
                        coef0 = temp[index,4])
  y_pred_temp <- predict(classifier_temp, newdata = test_tsne[,-3])
  cm_temp <- table(y_pred_temp,t(test_tsne[, 3]))
  temp_iv <- tikslumo_matai_param(cm_temp, length(test_tsne[,3]))
  temp$Accuracy[index] <- temp_iv$accuracy
  temp$F1[index] <- temp_iv$macroF1
  temp
}
par_tsne_svm_sigmoid <- temp[,c(3:6)]
write.csv(x = par_tsne_svm_sigmoid, file = "temp6.csv")

# Geriausias tikslumas buvo su branduoliu radial ir gamma = 0.1

classifier = svm(formula = Label ~ .,
                 data = train_tsne,
                 type = 'C-classification',
                 kernel = 'radial',
                 scale = FALSE,
                 gamma = 0.1)

y_pred2 = predict(classifier, newdata = test_tsne[,-3])

# Klasifikavimo matrica
(cm2 = table(y_pred2, t(test_tsne[, 3])))
tikslumo_matai(cm2, length(test_tsne[, 3]))

classifier
y_pred2_df <- as.data.frame(y_pred2)
rownames(y_pred2_df) <- 1:nrow(test_tsne)

y_pred2_df
par(mfrow=c(2, 2))
show_digit(test, 42)
show_digit(test, 53)
show_digit(test, 372)
show_digit(test, 396)
par(mfrow=c(1, 1))
##################################
# RF
##################################

train$Label <- as.factor(train$Label)
test$Label <- as.factor(test$Label)
ntree <- c(100,300,500)
mtry <- c(20,sqrt(nrow(train)),60) #sqrt(nrow(train)) - default <=> 40

temp <- tidyr::crossing(ntree, mtry)
for(index in 1:nrow(temp)){
  set.seed(67)
  classifier_temp = randomForest(
                            x = train[,-785],
                            y = train$Label,
                            ntree = as.numeric(temp[index, 1]),
                            mtry = as.numeric(temp[index, 2]))
  y_pred_temp <- predict(classifier_temp, newdata = test[,-785])
  cm_temp <- table(y_pred_temp,t(test[, 785]))
  temp_iv <- tikslumo_matai_param(cm_temp, nrow(test[,785]))
  temp$Accuracy[index] <- temp_iv$accuracy
  temp$F1[index] <- temp_iv$macroF1
  temp
}

par_RF <- temp
write.csv(x = par_RF, file = "temp7.csv")

# Geriusias buvo RF su ntree = 500, mtree nesvarbu. Jo modelis:
set.seed(67)
classifier = randomForest(x = train[,-785],
                          y = train$Label,
                          ntree = 500)

# testavimo aibes klasifikavimas
y_pred3 = predict(classifier, newdata = test[,-785])

# Sumaisymo matrica
(cm3 = table(as.character(y_pred3) ,test$Label))

tikslumo_matai(cm3, nrow(test[, 785]))

y_pred3
par(mfrow=c(2, 2))
show_digit(test, 42)
show_digit(test, 55)
show_digit(test, 287)
show_digit(test, 357)
par(mfrow=c(1, 1))

##################################
# RF t-SNE
##################################
train_tsne$Label <- as.factor(train_tsne$Label)
test_tsne$Label <- as.factor(test_tsne$Label)

temp <- tidyr::crossing(ntree, mtree)
for(index in 1:nrow(temp)){
  set.seed(67)
  classifier_temp = randomForest(
    Label ~ .,
    data = train_tsne,
    ntree = as.numeric(temp[index, 1]),
    mtree = as.numeric(temp[index, 2]))
  y_pred_temp <- predict(classifier_temp, newdata = test_tsne[,-3])
  cm_temp <- table(y_pred_temp,t(test_tsne[,3]))
  temp_iv <- tikslumo_matai_param(cm_temp, length(test_tsne[,3]))
  temp$Accuracy[index] <- temp_iv$accuracy
  temp$F1[index] <- temp_iv$macroF1
  temp
}

par_RF_tsne <- temp
write.csv(x = par_RF_tsne, file = "temp8.csv")

# Visi klasifikavo vienodai
set.seed(67)
classifier = randomForest(Label ~ ., data = train_tsne, ntree = 500)

# testavimo aibes klasifikavimas
y_pred4 = predict(classifier, newdata = test_tsne[,-3])

# Sumai?ymo matrica
(cm4 = table(as.character(y_pred4), test_tsne$Label))

tikslumo_matai(cm4, nrow(test_tsne[, 3]))


y_pred4_df <- as.data.frame(y_pred4)
rownames(y_pred4_df) <- 1:nrow(test_tsne)

y_pred4_df
par(mfrow=c(2, 2))
show_digit(test, 42)
show_digit(test, 35)
show_digit(test, 397)
show_digit(test, 396)
par(mfrow=c(1, 1))

##################################
# VIZUALIZAVIMAS CM
##################################

viz_scatter <- function(y_pred) {
  data_viz <- test_tsne

  data_viz$LabelPred <- y_pred
  
  data_viz$color <- ifelse(data_viz$Label == 5 & data_viz$LabelPred == 5, "TP",
                          ifelse(data_viz$Label == 5 & data_viz$LabelPred == 8, "FN",
                          ifelse(data_viz$Label == 8 & data_viz$LabelPred == 8, "TN",
                                 "FP")))


  ggplot(data_viz, aes(x = V1, y = V2, col = color)) + geom_point() +
    scale_color_manual(values = c("Red", "orange", "Blue", "Green"))
  
}

viz_scatter(y_pred1)
viz_scatter(y_pred2)
viz_scatter(y_pred3)
viz_scatter(y_pred4)


# 4.2: Holdout jau padarytas? nes tipo holdout tai ir yra tiesiog train test split
# apmokyti duomenys su train ir ivertinti su test?

##################################
# CV IR ROC
##################################
set.seed(67)
folds = createFolds(train_tsne$Label, k = 10)


# SVM t-SNE
cv = lapply(folds, function(x) {
  training_fold = train_tsne[-x, ]
  test_fold = train_tsne[x, ]
  
  classifier = svm(formula = Label ~ .,
                   data = train_tsne,
                   type = 'C-classification',
                   kernel = 'radial',
                   scale = FALSE,
                   gamma = 0.1)
  
  y_pred = predict(classifier, newdata = test_fold[-3])
  cm = table(test_fold[, 3], y_pred)
  accuracy = (cm[1,1] + cm[2,2]) / (cm[1,1] + cm[2,2] + cm[1,2] + cm[2,1])
  #cia analogiskai skaiciuokite taip pat ir precision, recall ir F1
  return(accuracy)
})
cv
(accuracy = mean(as.numeric(cv)))
(accuracy_sd =sd(as.numeric(cv)))


# RF t-SNE
cv = lapply(folds, function(x) {
  training_fold = train_tsne[-x, ]
  test_fold = train_tsne[x, ]
  
  set.seed(67)
  classifier = randomForest(x = training_fold[-3],
                            y = training_fold$Label,
                            ntree = 500)
  
  y_pred = predict(classifier, newdata = test_fold[-3])
  cm = table(test_fold[, 3], y_pred)
  accuracy = (cm[1,1] + cm[2,2]) / (cm[1,1] + cm[2,2] + cm[1,2] + cm[2,1])
  #cia analogiskai skaiciuokite taip pat ir precision, recall ir F1
  return(accuracy)
})
cv
(accuracy = mean(as.numeric(cv)))
(accuracy_sd =sd(as.numeric(cv)))

# Matome, kad su cross-validation rezultatai nesiskiria

#### ROC

roc_curve <- function(predictions, df) {
  pred <- prediction(as.numeric(predictions), as.numeric(df$Label))
  perf <- performance(pred,"tpr","fpr")
  plot(perf,colorize=TRUE, main = "ROC")
}

# ROC kazkaip galima su CV daryti bet nezinau kaip
roc_curve(y_pred1, test)
roc_curve(y_pred2, test_tsne)
roc_curve(y_pred3, test)
roc_curve(y_pred4, test_tsne)

# kodas suskaiciuoti AUC
auc(as.numeric(test_tsne$Label), as.numeric(y_pred3))

# bendras grafikas su visaiss ROC ir AUC
pred1 <- prediction(as.numeric(y_pred1), as.numeric(test$Label))
pred2 <- prediction(as.numeric(y_pred2), as.numeric(test_tsne$Label))
pred3 <- prediction(as.numeric(y_pred3), as.numeric(test$Label))
pred4 <- prediction(as.numeric(y_pred4), as.numeric(test_tsne$Label))
perf1 <- performance(pred1,"tpr","fpr")
perf2 <- performance(pred2,"tpr","fpr")
perf3 <- performance(pred3,"tpr","fpr")
perf4 <- performance(pred4,"tpr","fpr")

plot(perf1, col = "yellow",lwd = 2, main = "ROC")
plot(perf2, add = T, col="blue",lwd = 2,  main = "ROC")
plot(perf3, add = T, col="red", lwd = 2, main = "ROC")
plot(perf4, add = T, col="green",lwd = 2,  main = "ROC")
legend("bottomright", legend=c(paste('SVC, AUC= ', auc(as.numeric(test_tsne$Label),
                                                       as.numeric(y_pred1)), sep=""),
                               paste('SVC t-SNE, AUC= ', auc(as.numeric(test_tsne$Label),
                                                             as.numeric(y_pred2)), sep=""),
                               paste('RF, AUC= ', auc(as.numeric(test_tsne$Label),
                                                         as.numeric(y_pred3)), sep=""),
                               paste('RF t-SNE, AUC= ', auc(as.numeric(test_tsne$Label),
                                                         as.numeric(y_pred4)), sep="")),
       col=c("yellow","blue", "red", "green"), lty=1, cex=0.8)