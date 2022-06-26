library(ggplot2)

mtcars$gear <- factor(mtcars$gear,levels=c(3,4,5), 
          labels=c("3gears","4gears","5gears")) 
mtcars$am <- factor(mtcars$am,levels=c(0,1), labels=c("Automatic","Manual"))
mtcars$cyl <- factor(mtcars$cyl,levels=c(4,6,8), labels=c("4cyl","6cyl","8cyl")) 

qplot(hp, mpg, data=mtcars, shape=am, color=am, facets=gear~cyl, size=I(3),
      xlab="Horsepower", ylab="Miles per Gallon") 

qplot(gear, mpg, data=mtcars, 
      geom=c("boxplot", "jitter"), fill=gear, 
      main="Mileage by Gear Number", xlab="", ylab="Miles per Gallon") 

#######################################################################

set.seed(1410)
dsmall <- diamonds[sample(nrow(diamonds), 100), ]

qplot(carat, price, data = dsmall, colour = color)

qplot(carat, price, data = dsmall, shape = cut)

qplot(carat, price, data = diamonds, alpha = I(1/10) )
qplot(carat, price, data = diamonds, alpha = I(1/100) )

#######################################################################

qplot(color, price / carat, data = diamonds, geom = "jitter")

qplot(color, price / carat, data = diamonds, geom = "jitter", alpha = I(1 / 5) )
qplot(color, price / carat, data = diamonds, geom = "jitter", alpha = I(1 / 50) )


qplot(carat, data = diamonds, geom = "histogram", binwidth = 0.1, xlim = c(0,3))
qplot(carat, data = diamonds, geom = "histogram", binwidth = 0.01, xlim = c(0,3))

qplot(carat, data = diamonds, geom = "density" , colour = color)
qplot(carat, data = diamonds, geom = "histogram" , fill = color)


#######################################################################





