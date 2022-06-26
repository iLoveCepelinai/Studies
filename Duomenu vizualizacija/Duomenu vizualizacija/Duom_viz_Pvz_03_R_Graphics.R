library(ggplot2)

ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point()

ggplot(mpg, aes(displ, hwy)) +
  geom_point()

#############################################


ggplot(mpg, aes(displ, hwy, colour = class)) + 
  geom_point()

ggplot(mpg, aes(displ, hwy)) + 
  geom_point(colour = "blue")

#############################################

p <- ggplot(mpg, aes(displ, hwy))
p + geom_point(colour = "darkblue")

p + geom_point(aes(colour = "darkblue"))

#############################################

ggplot(mpg, aes(displ, hwy)) + 
  geom_point() + 
  geom_smooth()
#> `geom_smooth()` using method = 'loess' and formula 'y ~ x'

ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  geom_smooth(span = 0.2)

ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  geom_smooth(span = 1)

##############################################

ggplot(mpg, aes(drv, hwy)) + geom_boxplot()

ggplot(mpg, aes(drv, hwy)) + geom_violin()

############################################

depth_dist <- ggplot(diamonds, aes(depth)) +
  xlim(58, 68)

depth_dist + 
  geom_histogram(aes(y = ..density..), binwidth = 0.1) +
  facet_grid(cut ~ .)

##############################################

df <- data.frame(x = rnorm(2000), y = rnorm(2000))
norm <- ggplot(df, aes(x, y))
norm + geom_point(alpha=1/3)
norm + geom_point(alpha=1/5)
norm + geom_point(alpha=1/10)

###############################################


ggplot(mpg, aes(displ, colour = drv)) + 
  geom_freqpoly(binwidth = 0.5)

ggplot(mpg, aes(displ, fill = drv)) + 
  geom_histogram(binwidth = 0.5) + 
  facet_wrap(~drv, ncol = 1)







