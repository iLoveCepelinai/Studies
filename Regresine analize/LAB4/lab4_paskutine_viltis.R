library(readr)
library(plyr)
library(survival)
library(MASS)
library(survminer)
library(ggplot2)
library(eha) # coxreg

#######################################
# Duomenu tvarkymas
#######################################

a <- read_csv("Colorectal Cancer Gene Expression Data.csv")
b <- read_csv("Colorectal Cancer Patient Data.csv")

at <- as.data.frame(t(a))
at <- at[-c(1,2),]

b<-b[-63,-1]

at[] <- lapply(at, function(x) as.numeric(as.character(x)))
at$ID_REF <- rownames(at)
at <- at[,c(1936,1:1935)]

data1 <- join(b, at, by = "ID_REF")
data1 <- data1[,c(2,6,7,11, 14, 18, 20, 21, 25, 26, 27, 28, 29)]

names(data1)[c(1:3)] <- c("Age", "DFS", "Event")

#data1$DukesStage <- as.factor(data1$DukesStage)
#data1$Gender <- as.factor(data1$Gender)
#data1$Location <- as.factor(data1$Location)
#data1$Adj_Radio <- as.factor(data1$Adj_Radio)
#ata1$Adj_Chem <- as.factor(data1$Adj_Chem)


#######################################
# Duomenu vizualizacija
#######################################

ggplot(data = data1, aes(x=Age, y=DFS, color = as.factor(Event))) +
  geom_point(alpha = 0.9, size = 5) + theme_bw() + 
  labs(y = "Laikas iki ligos (mėnesiais)", x = "Amžius") +
  scale_color_discrete(name = "Įvykis")

ggplot(data = data1, aes(x=V2, y=DFS, color = as.factor(Event))) +
  geom_point(alpha = 0.9, size = 5)+ theme_bw()+ 
  labs(y = "Laikas iki ligos (mėnesiais)", x = "V2 (1007_s_at) geno raiškos lygis")+
  scale_color_discrete(name = "Įvykis") 

ggplot(data = data1, aes(x=V5, y=DFS, color = as.factor(Event))) +
  geom_point(alpha = 0.9, size = 5)+ theme_bw()+ 
  labs(y = "Laikas iki ligos (mėnesiais)", x = "V5 (1255_g_at) geno raiškos lygis")+
  scale_color_discrete(name = "Įvykis")

ggplot(data = data1, aes(x=V9, y=DFS, color = as.factor(Event))) +
  geom_point(alpha = 0.9, size = 5)+ theme_bw()+ 
  labs(y = "Laikas iki ligos (mėnesiais)", x = "V9 (1405_i_at) geno raiškos lygis")+
  scale_color_discrete(name = "Įvykis")

ggplot(data = data1, aes(x=V12, y=DFS, color = as.factor(Event))) +
  geom_point(alpha = 0.9, size = 5)+ theme_bw()+ 
  labs(y = "Laikas iki ligos (mėnesiais)", x = "V12 (1487_at) geno raiškos lygis")+
  scale_color_discrete(name = "Įvykis")

ggplot(data = data1, aes(x=V16, y=DFS, color = as.factor(Event))) +
  geom_point(alpha = 0.9, size = 5)+ theme_bw()+ 
  labs(y = "Laikas iki ligos (mėnesiais)", x = "V16 (1552258_at) geno raiškos lygis")+
  scale_color_discrete(name = "Įvykis")

ggplot(data = data1, aes(x=V18, y=DFS, color = as.factor(Event))) +
  geom_point(alpha = 0.9, size = 5)+ theme_bw()+ 
  labs(y = "Laikas iki ligos (mėnesiais)", x = "V18 (1552263_at) geno raiškos lygis")+
  scale_color_discrete(name = "Įvykis")

ggplot(data = data1, aes(x=V19, y=DFS, color = as.factor(Event))) +
  geom_point(alpha = 0.9, size = 5)+ theme_bw()+ 
  labs(y = "Laikas iki ligos (mėnesiais)", x = "V19 (1552264_a_at) geno raiškos lygis")+
  scale_color_discrete(name = "Įvykis")

ggplot(data = data1, aes(x=V20, y=DFS, color = as.factor(Event))) +
  geom_point(alpha = 0.9, size = 5)+ theme_bw()+ 
  labs(y = "Laikas iki ligos (mėnesiais)", x = "V20 (1552266_at) geno raiškos lygis")+
  scale_color_discrete(name = "Įvykis")

#write.csv(data1, "vezys.csv", row.names = F)
data1 <- data1[data1$V2 > 8,]


# Modelio tinkamumas (parametrinis ar semiparametrinis)
fit.cr <- coxreg(Surv(DFS, Event) ~ Age+V2+V5+V9+V11+V12+V16+V17+V18+
                   V19+V20, data1)
fit.we <- phreg(Surv(DFS, Event) ~ Age+V2+V5+V9+V11+V12+V16+V17+V18+
                  V19+V20, data1)
fit.ev <- phreg(Surv(DFS, Event) ~ Age+V2+V5+V9+V11+V12+V16+V17+V18+
                  V19+V20, dist = "ev", data1)
check.dist(fit.cr, fit.we)
check.dist(fit.cr, fit.ev)

AIC(fit.cr)
AIC(fit.we)
AIC(fit.ev)

#################################
# Modelio kurimas
#################################

#cox1 <- coxph(Surv(DFS, Event) ~ Age + DukesStage + Gender + Location +
#                Adj_Radio + Adj_Chem +V1+V2+V3+V4+V5+V6+V7+V8+V9+V10+V11+
#                V12+V13+V14+V15+V16+V17+V18+V19+V20, data1)
#cox.zph(cox1)
#summary(cox1)

#stepAIC(cox1)

cox1 <- coxph(Surv(DFS, Event) ~ Age+V2+V5+V9+V11+V12+V16+V17+V18+
                V19+V20, data1)
cox.zph(cox1)
summary(cox1)

outliers <-2/sqrt(nrow(data1))
outliers

dfbeta <- residuals(cox1, type="dfbeta")

par(mfrow=c(3, 3))
for (j in 1:9) {
  plot(dfbeta[, j],
       ylab=names(coef(cox1))[j])
  abline(h=0, lty=2)
}
par(mfrow=c(1, 1))

data2 <- data1[-which(abs(dfbeta[,3]) > outliers),]

cox2 <- coxph(Surv(DFS, Event) ~ Age+V2+V5+V9+V11+V12+V16+V17+V18+V19+V20, data2)
cox.zph(cox2)
summary(cox2)

stepAIC(cox2)

cox3 <- coxph(Surv(DFS, Event) ~ V2+V9+V12+V19, data2)
cox.zph(cox3)
summary(cox3)

par(mfrow=c(2, 2))
res <- residuals(cox3, type="martingale")
X <- as.matrix(data2[, c("V2", "V9")])
for (j in 1:2) { # residual plots
  plot(X[, j], res, xlab=c("V2", "V9")[j],
       ylab="residuals")
  abline(h=0, lty=2)
  lines(lowess(X[, j], res, iter=0))
}

b <- coef(cox3)[c(1:2)]
for (j in 1:2) { 
  plot(X[, j], b[j]*X[, j] + res, xlab=c("V2", "V9")[j],
       ylab="component+residual")
  abline(lm(b[j]*X[, j] + res ~ X[, j]), lty=2)
  lines(lowess(X[, j], b[j]*X[, j] + res, iter=0))
}
par(mfrow=c(1, 1))

####

par(mfrow=c(2, 2))
res <- residuals(cox3, type="martingale")
X <- as.matrix(data2[, c("V12", "V19")])
for (j in 1:2) { # residual plots
  plot(X[, j], res, xlab=c("V12", "V19")[j],
       ylab="residuals")
  abline(h=0, lty=2)
  lines(lowess(X[, j], res, iter=0))
}

b <- coef(cox3)[c(3:4)]
for (j in 1:2) { 
  plot(X[, j], b[j]*X[, j] + res, xlab=c("V12", "V19")[j],
       ylab="component+residual")
  abline(lm(b[j]*X[, j] + res ~ X[, j]), lty=2)
  lines(lowess(X[, j], b[j]*X[, j] + res, iter=0))
}
par(mfrow=c(1, 1))

#####################################################
# Rezultatai
#####################################################
summary(cox3)

data3 <- data2

data3$V2_cat <- factor(ifelse(data3$V2 < 10.2, "Mažiau už 10.2",
                              "Daugiau arba lygu už 10.2"))
table(data3$V2_cat)

data3$V9_cat <- factor(ifelse(data3$V9 < 8, "Mažiau už 8",
                              "Daugiau arba lygu už 8"))
table(data3$V9_cat)

data3$V12_cat <- factor(ifelse(data3$V12 < 8.3, "Mažiau už 8.3",
                              "Daugiau arba lygu už 8.3"))
table(data3$V12_cat)

data3$V19_cat <- factor(ifelse(data3$V19 < 6.7, "Mažiau už 4.2",
                               "Daugiau arba lygu už 4.2"))
table(data3$V19_cat)


dev.off()
ggsurvplot(survfit(Surv(DFS, Event) ~ V2_cat + V9_cat + V12_cat + V19_cat,
                   data = data3),
           legend.labs = c(">= 10,2", "< 10,2"), ggtheme = theme_bw(), 
           xlab = "Laikas mėnesiais", ylab = "Ligos nesugrįžimo tikimybė",
           subtitle = "V2 (1007_s_at) genas")

ggsurvplot(survfit(Surv(DFS, Event) ~ V9_cat, data = data3),
           legend.labs = c(">= 8", "< 8"), ggtheme = theme_bw(), 
           xlab = "Laikas mėnesiais", ylab = "Ligos nesugrįžimo tikimybė",
           subtitle = "V9 (1405_i_at) genas")

ggsurvplot(survfit(Surv(DFS, Event) ~ V12_cat, data = data3),
           legend.labs = c(">= 8,3", "< 8,3"), ggtheme = theme_bw(), 
           xlab = "Laikas mėnesiais", ylab = "Ligos nesugrįžimo tikimybė",
           subtitle = "V12 (1487_at) genas")

ggsurvplot(survfit(Surv(DFS, Event) ~ V19_cat, data = data3),
           legend.labs = c(">= 6,7", "< 6,7"), ggtheme = theme_bw(), 
           xlab = "Laikas mėnesiais", ylab = "Ligos nesugrįžimo tikimybė",
           subtitle = "V19 (1007_s_at) genas")
