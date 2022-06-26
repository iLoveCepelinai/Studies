
plot( cars$dist ~ cars$speed )
plot( cars$dist, cars$speed )
cars
#####################################################

plot(cars$dist~cars$speed, 
     main="Sąryšis tarp atstumo ir greičio", 
     xlab="Greitis", ylab="Nuvažiuotas atstumas", 
     xlim=c(0,30), ylim=c(0,140), 
     xaxs="i", yaxs="i"  , # ašių tipas internal  
     col="red", pch=19) # simbolis 

#####################################################

# iskeliame duomenis: Import Dataset

Protein <- read.csv("Protein.txt", sep="")

plot(Protein$RedMeat, Protein$Eggs, xlim=c(0, 20), ylim=c(0,5), 
     xlab="Raudona mėsa", ylab="Kiaušiniai")

plot(Protein$RedMeat, Protein$Eggs, xlim=c(0, 20), ylim=c(0,5), 
     xlab="Raudona mėsa", ylab="Kiaušiniai", pch=19 )   # arba pch= " * "

#####################################################

plot(Protein$RedMeat, Protein$Eggs, cex=0.8, pch=25, col="blue", bg="red", xlab="Raudona mėsa", ylab="Kiaušiniai")
grid()
title(main="Baltymų suvartojimas (25 Europos šalys)")
abline(v=mean(Protein$RedMeat), 
       lty=2, col="blue")
abline(h=mean(Protein$Eggs), 
       lty=1, col="blue")

#####################################################

points(mean(Protein$RedMeat), mean(Protein$Eggs), pch=23, col="black", bg="yellow", cex=2)
text(mean(Protein$RedMeat), min(Protein$Eggs), paste("Vidurkis:",round(mean(Protein$RedMeat),2)))
text(min(Protein$RedMeat), mean(Protein$Eggs), paste("Vidurkis:",round(mean(Protein$Eggs),2)), adj=c(-1,1))

#####################################################

library( RcmdrMisc)
par(mfrow=c(2,2))                   #    dvi eilutės, du stulpeliai
.x <- 0:10
plotDistr(.x,   dbinom(.x, size=10, prob=0.75)  , xlab="n=10;  p=0,75;  EX=7,5;  DX=1,875", ylab="Tikimybės", 
          main="X~B(10; 0,75)", discrete=TRUE)
remove(.x)
.x <- 0:10
plotDistr(.x, dbinom(.x, size=10, prob=0.5), xlab="n=10;  p=0,5;  
     EX=5;  DX=2,5", ylab="Tikimybės", 
          main="X~B(10; 0,5)", discrete=TRUE)
remove(.x)
.x <- 0:10
plotDistr(.x, dbinom(.x, size=10, prob=0.25), xlab="n=10;  p=0,25;  
      EX=2,5;  DX=1,875", ylab="Tikimybės", 
          main="X~B(10; 0,25)", discrete=TRUE)
remove(.x)
par(mfrow=c(1,1))

#####################################################
library(lattice)
attach(mtcars)
?mtcars
mtcars

gear.f<-factor(gear,levels=c(3,4,5), labels=c("3gears","4gears","5gears"))
cyl.f <-factor(cyl,levels=c(4,6,8), labels=c("4cyl","6cyl","8cyl")) 
xyplot(mpg~wt|cyl.f+gear.f,  
       main="Scatterplots by Cylinders and Gears",
       ylab="Miles per Gallon", xlab="Car Weight")
detach(mtcars)

#####################################################

data(Oats, package = "MEMSS")
tp1.oats <-
  xyplot(yield ~ nitro | Variety + Block, data = Oats, type = "o")
dim(tp1.oats)
dimnames(tp1.oats)
plot(tp1.oats)

######################################################
tp1.oats <- xyplot(yield ~ nitro | Variety + Block, data = Oats, type = "o")
plot(tp1.oats[, 1])
plot(tp1.oats[1,])
plot(tp1.oats[1,1])

#####################################################

xyplot(yield ~ nitro | Variety + Block, data = Oats, type = "o", as.table=TRUE)

update(tp1.oats, aspect = "xy", layout = c(0, 18),
       between = list(x = c(0, 0, 0.5), y = 0.5))

#####################################################

attach(mtcars)

gear.f<-factor(gear,levels=c(3,4,5), labels=c("3gears","4gears","5gears"))

cyl.f <-factor(cyl,levels=c(4,6,8), labels=c("4cyl","6cyl","8cyl")) 

am.f <-factor(am,levels=c(0,1),
              labels=c("automatic","manual")) 
xyplot(mpg~wt|cyl.f*gear.f,
       main="Scatterplots by Cylinders and Gears",
       ylab="Miles per Gallon", xlab="Car Weight", groups=am.f)

xyplot(mpg~wt|cyl.f*gear.f,
       main="Scatterplots by Cylinders and Gears",
       ylab="Miles per Gallon", xlab="Car Weight", groups=am.f, auto.key = list(space = "right"))

detach(mtcars)

#####################################################

attach(Oats)

key.variety <- list(space = "right", 
                    text = list(levels(Oats$Variety)),
                    points = list(pch = 1:3, col = "black"))

xyplot(yield ~ nitro | Block, Oats, aspect = "xy", type = "o", groups = Variety, key = key.variety, 
       lty = 1, pch = 1:3, col.line = "darkgrey", 
       col.symbol = "black",
       xlab = "Nitrogen concentration (cwt/acre)",
       ylab = "Yield (bushels/acre)",
       main = "Yield of three varieties of oats",
       sub = "A 3 x 4 split-plot experiment with 6 blocks")

detach(Oats)

###########################################################

barchart(Class ~ Freq | Sex + Age, 
         data = as.data.frame(Titanic),
         groups = Survived, stack = TRUE, layout = c(4, 1),
         auto.key = list(title = "Survived", columns = 2))


barchart(Class ~ Freq | Sex + Age, data = as.data.frame(Titanic),
         groups = Survived, stack = TRUE, layout = c(4, 1),
         auto.key = list(title = "Survived", columns = 2),
         scales = list(x = "free"))

#####################################################################
bc.titanic <- barchart(Class ~ Freq | Sex + Age,   
                       as.data.frame(Titanic),
                       groups = Survived, stack = TRUE, layout = c(4, 1),
                       auto.key = list(title = "Survived", columns = 2),
                       scales = list(x = "free"))


bc.titanic
update(bc.titanic, panel = panel.barchart)

update(bc.titanic,
       panel = function(...) {
         panel.grid(h = 0, v = -1, col="grey",   
                    lty=1, ltw=1)
         panel.barchart(...)
       })

update(bc.titanic,
       panel = function(..., border) {
         panel.barchart(..., border = "transparent")
       })

###############################################################
library(lattice)
show.settings()

my.theme <- trellis.par.get() 

my.theme$plot.line$lwd <- 2 
my.theme$plot.symbol$pch <- 16 
my.theme$superpose.symbol$pch <- rep(16, 7)
# įjungiam naujus nustatymus 
trellis.par.set(my.theme) 
show.settings() 

dev.off()   
show.settings() 

###############################################################

library(titanic)
grafikas <- barchart(Class ~ Freq | Sex + Age, data = as.data.frame(Titanic), groups = Survived, stack = TRUE,  layout = c(4, 1), auto.key = list(title = "Survived", columns = 2), scales = list(x = "free")) 
print(grafikas)
trellis.device(color = FALSE) 
print(grafikas)

###############################################################







