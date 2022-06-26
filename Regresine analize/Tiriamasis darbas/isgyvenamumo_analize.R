Sys.setlocale("LC_ALL","Lithuanian")
data <- read.csv("HRDataset.csv")

table(data$Termd, data$MaritalDesc)
table(data$Termd, data$RecruitmentSource)

library(tidyverse)
data <- data %>%
  mutate(RSource_joined = fct_lump(RecruitmentSource, n = 3,
                                   other_level = "Other"),
         Marital_joined = fct_lump(MaritalDesc, n=3, other_level="Other"))

set.seed(111)

data[c(1,4:14,22)] <- lapply(data[c(1,4:14,22)], factor)
############################################################################
# Išgyvenamumo analizė

library(survival)
library(survminer)
library(MASS)
cox1 <- coxph(Surv(dirbo_men, Termd) ~  Salary +
                Sex + Marital_joined + CitizenDesc + HispanicLatino + 
                RaceDesc + Department +
                RSource_joined + PerformanceScore + EngagementSurvey + 
                EmpSatisfaction + SpecialProjectsCount + DaysLateLast30 + 
                Absences + Age,
              data = data)

summary(cox1)

stepAIC(cox1, direction = "both")

cox2 <- coxph(formula = Surv(dirbo_men, Termd) ~ Marital_joined + Department + 
                RSource_joined + SpecialProjectsCount + DaysLateLast30 + 
                Absences + Age, data = data)
summary(cox2)

# PH prielaida
cox.zph(cox2)
ggcoxzph(cox.zph(cox2))


cox3 <- coxph(formula = Surv(dirbo_men, Termd) ~ Marital_joined + strata(Department) + 
                RSource_joined + SpecialProjectsCount + DaysLateLast30 + 
                Absences + Age, data = data)
summary(cox3)

# PH prielaida
cox.zph(cox3)

# Išskirtys
outliers <-2/sqrt(nrow(data))
outliers

ggcoxdiagnostics(cox3, type = "dfbeta",
                 linear.predictions = FALSE, ggtheme = theme_bw(), xlab = '',
                 ylab = '', main ='DFBETA liekanos') +
  geom_hline(yintercept=c(outliers,-outliers), color = 'darkred', linetype='dashed', size = 1)

####################################################################################
# Jei pašalinam išskirtis, marital_joined "other" lieka visi su termd=0,
# todėl nesivertina parametrai 

dfbeta <- residuals(cox3, type="dfbeta")
data2 <- data[which((abs(dfbeta[,3]) < outliers))
                                   ,]
table(data2$Marital_joined, data2$Termd)

cox4 <- coxph(formula = Surv(dirbo_men, Termd) ~ Marital_joined + strata(Department) + 
                RSource_joined + DaysLateLast30 + Age, data = data)
summary(cox4)

table(data$Marital_joined, data$Termd)

###################################################################

# Martingalų liekanos
par(mfrow=c(2, 2))
res <- residuals(cox4, type="martingale")
X <- as.matrix(data[, c("DaysLateLast30","Age")])
for (j in 1:2) { 
  plot(X[, j], res, xlab=c("Vėlavimas","Amžius")[j], 
       ylab="Martingalų liekanos")
  abline(h=0,lty=2, lwd=3, col='blue')
  lines(lowess(X[, j], res, iter=0), col='red', lwd=3)
}
b <- coef(cox4)[c(7,8)] 
for (j in 1:2) { 
  plot(X[, j], b[j]*X[, j] + res, xlab=c("Vėlavimas","Amžius")[j],
       ylab="Komponentė + martingalų liekanos")
  abline(lm(b[j]*X[, j] + res ~ X[, j]), lty=2, lwd=3, col='blue')
  lines(lowess(X[, j], b[j]*X[, j] + res, iter=0), col='red', lwd=3)
}
par(mfrow=c(1, 1))


################################################################################
#Bandome transformuoti DaysLateLast30

data$DaysLateLast30_mod <- sqrt(data$DaysLateLast30)
cox5 <- coxph(formula = Surv(dirbo_men, Termd) ~ Marital_joined + strata(Department) + 
                RSource_joined + DaysLateLast30_mod + Age, data = data)
cox.zph(cox5)

# Martingalų liekanos
par(mfrow=c(2, 2))
res <- residuals(cox5, type="martingale")
X <- as.matrix(data[, c("DaysLateLast30_mod","Age")])
for (j in 1:2) { 
  plot(X[, j], res, xlab=c("Vėlavimas","Amžius")[j], 
       ylab="Martingalų liekanos")
  abline(h=0,lty=2, lwd=3, col='blue')
  lines(lowess(X[, j], res, iter=0), col='red', lwd=3)
}
b <- coef(cox5)[c(7,8)] 
for (j in 1:2) { 
  plot(X[, j], b[j]*X[, j] + res, xlab=c("Vėlavimas","Amžius")[j],
       ylab="Komponentė + martingalų liekanos")
  abline(lm(b[j]*X[, j] + res ~ X[, j]), lty=2, lwd=3, col='blue')
  lines(lowess(X[, j], b[j]*X[, j] + res, iter=0), col='red', lwd=3)
}
par(mfrow=c(1, 1))

summary(cox5)

##############################################################################
# Grafikai kategoriniams
dev.off()
ggsurvplot(survfit(Surv(dirbo_men, Termd) ~ Marital_joined, data = data), 
           data=data, ggtheme = theme_bw(), pval=TRUE,
           xlab = "Laikas mėnesiais", ylab = "Tikimybė likti darbe")

ggsurvplot(survfit(Surv(dirbo_men, Termd) ~ RSource_joined, data = data),
           data=data, ggtheme = theme_bw(),pval=TRUE,
           xlab = "Laikas mėnesiais", ylab = "Tikimybė likti darbe")

##############################################################################
# Nenaudojant marital kovariantės

cox_1 <- coxph(formula = Surv(dirbo_men, Termd) ~ Department + RSource_joined + 
                 DaysLateLast30 + Age, data = data)

summary(cox_1)

# PH prielaida
cox.zph(cox_1)
ggcoxzph(cox.zph(cox_1))

cox_2 <- coxph(formula = Surv(dirbo_men, Termd) ~ strata(Department) + RSource_joined + 
                 DaysLateLast30 + Age, data = data)
summary(cox_2)

# PH prielaida
cox.zph(cox_2)

# Išskirtys
outliers <-2/sqrt(nrow(data))
outliers

ggcoxdiagnostics(cox_2, type = "dfbeta",
                 linear.predictions = FALSE, ggtheme = theme_bw(), xlab = '',
                 ylab = '', main ='DFBETA liekanos') +
  geom_hline(yintercept=c(outliers,-outliers), color = 'darkred', linetype='dashed', size = 1)

#-------------------------------------------------------------------------------
# Martingalų liekanos

data$DaysLateLast30_mod <- sqrt(data$DaysLateLast30)
cox_3 <- coxph(formula = Surv(dirbo_men, Termd) ~ strata(Department) + RSource_joined + 
                 DaysLateLast30_mod + Age, data = data)
cox.zph(cox_3)

# Martingalų liekanos
par(mfrow=c(2, 2))
res <- residuals(cox_3, type="martingale")
X <- as.matrix(data[, c("Age","DaysLateLast30_mod")])
for (j in 1:2) { 
  plot(X[, j], res, xlab=c("Amžius","Vėlavimas")[j], 
       ylab="Martingalų liekanos")
  abline(h=0, lty=2, lwd=3, col='blue')
  lines(lowess(X[, j], res, iter=0), col='red', lwd=3)
}
b <- coef(cox_3)[c(4,5)] 
for (j in 1:2) { 
  plot(X[, j], b[j]*X[, j] + res, xlab=c("Amžius","Vėlavimas")[j],
       ylab="Komponentė + martingalų liekanos")
  abline(lm(b[j]*X[, j] + res ~ X[, j]), lty=2, lwd=3, col='blue')
  lines(lowess(X[, j], b[j]*X[, j] + res, iter=0), col='red', lwd=3)
}
par(mfrow=c(1, 1))

summary(cox_3)

