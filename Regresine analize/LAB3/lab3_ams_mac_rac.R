library(readr)

##################################
# Sutvarkytu duomenu lenteles kurimas (galima praleisti jei turima autos2)
##################################
autos <- read_csv("autos.csv")
autos1 <- autos1[,c(5,7:10,12,14,16)]
autos1 <- autos[autos$offerType=="Angebot",]
autos1 <- autos[autos$seller=="privat",]
autos1 <- autos1[autos1$price < 4000000 & autos1$price > 100,]
autos1 <- autos1[autos1$powerPS < 500 & autos1$powerPS > 30,]

autos1$amzius <- 2016 - autos1$yearOfRegistration # nes rinkta 2016
autos1 <- autos1[,-3]

sum(is.na(autos1))
autos2 <- na.omit(autos1)

hist(autos2$price)
boxplot(autos2$price)


quantile(autos1$price, probs = c(0, 0.5, 0.75, 0.95, 1))
autos2 <- autos2[autos2$price < 20000,]
hist(autos2$price, xlab = "Kaina", ylab = "Dažnis",
     main = "Automobilių kainų histograma")

write.csv(autos2, file = "autos2.csv", row.names = F)


##################################
# Imties emimas
##################################

autos2 <- read_csv("autos2.csv")

set.seed(67)
sample_autos<-autos2[sample(1:nrow(autos2), 20000, replace=FALSE),]

table(sample_autos$vehicleType)
table(sample_autos$gearbox)
table(sample_autos$fuelType)
table(sample_autos$notRepairedDamage)

sample_autos2 <- sample_autos[sample_autos$fuelType == "benzin" | 
                              sample_autos$fuelType == "diesel" |
                              sample_autos$fuelType == "lpg",]

cor(sample_autos2[c("price", "powerPS", "kilometer", "amzius")])
boxplot(sample_autos2$price)

##################################
# Modelio taikymas
##################################

lm <- lm(price ~ ., data = sample_autos2)
summary(lm)

plot(rstudent(lm), xlab= "",ylab="", main = "Standartizuotų liekanų grafikas")
plot(cooks.distance(lm))


library(quantreg)
modelis1 <- rq(price ~ vehicleType + gearbox + powerPS + kilometer + fuelType + 
                 notRepairedDamage + amzius + vehicleType * amzius + amzius * kilometer,
                 data=sample_autos2, tau = 0.25)
modelis2 <- rq(price ~ vehicleType + gearbox + powerPS + kilometer + fuelType + 
                 notRepairedDamage + amzius + vehicleType * amzius + amzius * kilometer,
               data=sample_autos2, tau = 0.5)
modelis3 <- rq(price ~ vehicleType + gearbox + powerPS + kilometer + fuelType + 
                 notRepairedDamage + amzius + vehicleType * amzius + amzius * kilometer,
               data=sample_autos2, tau = 0.75)

summary(modelis1)
summary(modelis2)
summary(modelis3)

# Pasalinam amziu
modelis3 <- rq(price ~ vehicleType + gearbox + powerPS + kilometer + fuelType + 
                 notRepairedDamage  + vehicleType:amzius + amzius:kilometer,
               data=sample_autos2, tau = 0.75)

summary(modelis3)

##################################
# Vizualizavimas
##################################

library(ggplot2)

ggplot(sample_autos2, aes(amzius,price)) + geom_point() + 
  geom_abline(intercept=coef(modelis1)[1], slope=coef(modelis1)[15], color = "blue") + 
  geom_abline(intercept=coef(modelis2)[1], slope=coef(modelis2)[15], color = "green") +
  xlab("Amžius") + ylab("Kaina")


qs <- 1:9/10

qr <- rq(price ~ amzius + kilometer + powerPS + amzius*kilometer,
          data=sample_autos, tau = qs)
plot(summary(qr))


###############################################
# Tiesine regresija pries kvantiliu regresija
###############################################

# AMZIUS
plot(price ~ amzius, data = sample_autos2, pch = 16,
     xlab = "Amžius", ylab = "Kaina", main = "kaina ~ amžius")
abline(lm(price ~ amzius, data = sample_autos2), col = "red", lty = 2)
abline(rq(price ~ amzius, data = sample_autos2, tau = 0.5), col = "blue", lty = 2)
legend("topright", legend = c("lm", "rq"), col = c("red", "blue"), lty = 2)

# RIDA
plot(price ~ kilometer, data = sample_autos2, pch = 16,
     xlab = "Rida", ylab = "Kaina", main = "kaina ~ rida")
abline(lm(price ~ kilometer, data = sample_autos2), col = "red", lty = 2)
abline(rq(price ~ kilometer, data = sample_autos2, tau = 0.5), col = "blue", lty = 2)
legend("topright", legend = c("lm", "rq"), col = c("red", "blue"), lty = 2)


# GALIA
plot(price ~ powerPS, data = sample_autos2, pch = 16,
     xlab = "Galia", ylab = "Kaina", main = "kaina ~ galia")
abline(lm(price ~ powerPS, data = sample_autos2), col = "red", lty = 2)
abline(rq(price ~ powerPS, data = sample_autos2, tau = 0.5), col = "blue", lty = 2)
legend("topright", legend = c("lm", "rq"), col = c("red", "blue"), lty = 2)

##################################
# Prognozavimas
##################################
lr <- lm(price ~ vehicleType + gearbox + powerPS + kilometer + fuelType + 
           notRepairedDamage + amzius + vehicleType * amzius + amzius * kilometer,
         data=sample_autos2)

plot(sample_autos$price[1:30], lwd=2, xlab = "Indeksas",
     ylab="Kaina", main = "Kainų prognozavimas")
lines(modelis1$fitted.values[1:30], col="blue")
lines(modelis2$fitted.values[1:30], col="green")
lines(modelis3$fitted.values[1:30], col="red")
lines(lr$fitted.values[1:30], lty = 2)
legend("topleft", legend = c("0.25Q", "0.5Q", "0.75Q"), 
       col = c("blue", "green", "red"), lty = 1)

##########################################
# Modelis, kai palikti ir brangiausi automobiliai
##########################################
autos <- read_csv("autos.csv")
autos1 <- autos[autos$offerType=="Angebot",]
autos1 <- autos[autos$seller=="privat",]
autos1 <- autos1[autos1$price < 4000000 & autos1$price > 100,]
autos1 <- autos1[autos1$powerPS < 500 & autos1$powerPS > 30,]
autos1 <- autos1[,c(5,7:10,12,14,16)]

autos1$amzius <- 2016 - autos1$yearOfRegistration # nes rinkta 2016
autos1 <- autos1[,-3]
autos3 <- na.omit(autos1)
autos3 <- autos3[autos3$price < 100000,]

set.seed(67)
sample_autos3<-autos3[sample(1:nrow(autos3), 20000, replace=FALSE),]
sample_autos4 <- sample_autos3[sample_autos3$fuelType == "benzin" | 
                                sample_autos3$fuelType == "diesel" |
                                sample_autos3$fuelType == "lpg",]


hist(sample_autos4$price, xlab = "Kaina", ylab = "Dažnis",
     main = "Automobilių kainų histograma")
plot(sample_autos4$price)

qr1_ <- rq(price ~ vehicleType + gearbox + powerPS + kilometer + fuelType + 
            notRepairedDamage + amzius + vehicleType * amzius + amzius * kilometer,
          data=sample_autos4, tau = 0.25)
qr2_ <- rq(price ~ vehicleType + gearbox + powerPS + kilometer + fuelType + 
                 notRepairedDamage + amzius + vehicleType * amzius + amzius * kilometer,
               data=sample_autos4, tau = 0.5)
qr3_ <- rq(price ~ vehicleType + gearbox + powerPS + kilometer + fuelType + 
            notRepairedDamage + amzius + vehicleType * amzius + amzius * kilometer,
          data=sample_autos4, tau = 0.75)

lr_ <- lm(price ~ vehicleType + gearbox + powerPS + kilometer + fuelType + 
           notRepairedDamage + amzius + vehicleType * amzius + amzius * kilometer,
         data=sample_autos4)


plot(sample_autos4$price[1:30], lwd=2, xlab = "Indeksas",
     ylab="Kaina", main = "Kainų prognozavimas")
lines(qr1_$fitted.values[1:30], col="blue")
lines(qr2_$fitted.values[1:30], col="green")
lines(qr3_$fitted.values[1:30], col="red")
lines(lr_$fitted.values[1:30], lty = 2)
legend("topleft", legend = c("0.25Q", "0.5Q", "0.75Q"), 
       col = c("blue", "green", "red"), lty = 1)

