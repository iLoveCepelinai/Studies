#vadovelyje visai solidziu pavyzdiu su tavo tevu atributu pavadinimais ir panasiai

#keiciame vektoriu vienmati i matrica
z <- 1:6
dim(z) <- c(3,2)
z[5]#skaiciuojamas ne pagal eilutes, o pagal stulpelius (nes cia tiesiog vektorius)

x<-factor(c('a','b','b','a'))
x
typeof(x)

#jein nori uznulinti matrica, reikia tada taip:
x<-1:4
dim(x) <- c(2,2)
x[]<-0
x

a<-matrix(1:9, nrow = 3)
a
a[1:2,]
colnames(a) <- c('A','B','C')

#drop = false - palaiko matricos dimensija (arba data.frame)

greet <- function(name, birthday = FALSE) {
  paste0(
    "Hi ", name,
    if (birthday) " and HAPPY BIRTHDAY"
  )
}
greet("Maria", FALSE)
#> [1] "Hi Maria"
greet("Jaime", TRUE)
#> [1] "Hi Jaime and HAPPY BIRTHDAY"

ifelse(c(T,F,T),0,1)

26%%5#liekana

#for loops

#for (item in vector) {perform action}

#praktika su apply
#lapply(X=duomenys, FUN = )
lapply(X=c(1:dim(mymatrix)[2]), FUN = function(i){sum(mymatrix[,i])})

rep(x=1, times = 10)
seq(from = 1, to = 2, by = 0.1)
