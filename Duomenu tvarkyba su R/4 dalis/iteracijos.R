#Geriausia ziurek knyga
#https://r4ds.had.co.nz/relational-data.html

n <- 10^6

df <- tibble(
  a = rnorm(n),
  b = rnorm(n),
  c = rnorm(n),
  d = rnorm(n)
)

#Pirma reikia pasidaryti tuscia konteineri!!!
output <- vector("double", ncol(df))

for (i in seq_along(df)) {
  output[[i]] <- median(df[[i]])
}

output

#Kai nezinomas ilgis duomenu. Svarbu nejungti cikle
means <- c(0, 1, 2)

out <- vector("list", length(means))
for (i in seq_along(means)) {
  n <- sample(100, 1)
  out[[i]] <- rnorm(n, means[[i]])
}
str(out)


#Taip par yra medziai
map_dbl(df, mean)

models <- mtcars %>% 
  split(.$cyl) %>% 
  map(function(df) lm(mpg ~ wt, data = df))
models


#Bet geriau:
models <- mtcars %>% 
  split(.$cyl) %>% 
  map(~lm(mpg ~ wt, data = .))
models

rez <- summary(models[[1]])
rez$r.squared


#Bet ar map greiciau uz sapply
kiek_laik_vykdo <- function(f){
  pradzia <- now()
  f()
  pabaiga <- now()
  pabaiga - pradzia
}

m <- 100
mapVektorius <- vector(mode = "double", length = m)
sapplyVektorius <- vector(mode = "double", length = m)

for(i in c(1:m)){
  sapplyVektorius<-kiek_laik_vykdo(function(){sapply(X=df, FUN = mean, na.rm = T)})
  mapVektorius<-kiek_laik_vykdo(function(){map_dbl(df, mean, na.rm = T)})
}

quantile(mapVektorius)
quantile(sapplyVektorius)

kiek_laik_vykdo(function(){map_dbl(df, mean, na.rm = T)})
kiek_laik_vykdo(function(){sapply(X=df, FUN = mean, na.rm = T)})


map_dbl(df, mean, na.rm = T)
sapply(X=df, FUN = mean, na.rm = T)
