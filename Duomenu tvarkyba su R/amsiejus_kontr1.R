#Kontrolinis 1. Matas Amsiejus. DM 2 gr. 3 kursas
#1 variantas

#1.
x<-c(1:12)
#naudojant dim
dim(x)<-c(3,4)
x
#kuriant matrica
x<-matrix(x,nrow = 3, ncol = 4, byrow = T)
x
#priskirkime vardus (renkiniu budu)
rownames(x) <- c("E1","E2","E3")
colnames(x) <- c("C1","C2","C3","C4")
x
#jei matrica ilga, tai:
pavc <- paste("C", 1:100, sep = "")
pavc
#musu pavyzdziui:
pavc <- paste("C", 1:ncol(x), sep = "")
colnames(x) <- pavc
x



#2.
#a)
A <- as.matrix(mtcars)
dimnames(A) <- NULL
A
#kiekvienos eilutes variacijos koeficientas:
apply(A, 1, function(x) sd(x) / mean(x))

#stulkpelio elem. sk, virsijanti vidurki
apply(A, 2, function(x) sum((mean(x) < x), na.rm = T))

#b)
#kiekvienos eilutes variacijos koeficientas:
varkoef<-c()
for(eil in 1:nrow(A)){
  eilut<-c()
  for(stulp in 1:ncol(A)){
    elem<-A[eil,stulp]
    eilut<-c(eilut,elem)
  }
  varkoef<-c(varkoef, sd(eilut) / mean(eilut))
}
varkoef

#stulkpelio elem. sk, virsijanti vidurki
daugvid<-c()
for(stulp in 1:ncol(A)){
  stulpel<-c()
  for(eil in 1:nrow(A)){
    stulpel<-c(stulpel,A[eil,stulp])
  }
  st_vid<-mean(stulpel)
  daugvid<-c(daugvid, sum((st_vid<stulpel),na.rm = T))
}
daugvid

#c)
median(A[,1])#gauname tokia mediana
A[median(A[,1])<A[,1],1]


#3.
df<-datasets::iris
#a)naudoti lapply ir typeof
lapply(df, typeof)

sapply(df, typeof)
#sapply funkcijos rezultatas is esmes nesiskiria, taciau skiriasi grazinimo forma. Sapply funkcijoje yra grazinamas
#lengviau vartotojui suprantamas vektorius vietoje saraso (lapply)

#b)
apply(df[ , -5], MARGIN =2, FUN = mean)

sapply(df[ , -5], FUN = mean)
#apply veikia, nes pries atliekant skaiciavimus, ji paduota dataframe konvertuoja i matrica(masyva) (as.matrix)
#nurasyta nuo ?apply

df1 <- df
df2 <- df
df1 <- df1[-c(length(df))]
df2[[length(df)]] <- NULL

#su df1 atimame paskutine eilute (length duoda paskutinio elemento indeksa (teoriskai ilgi), - atima, c siuo atveju
#nieko nekeicia, nes mums tereikia stulpelio numerio. Su NULL mes paprasciausiai padarome visa stulpeli NULL, tuscia,
#todel stulpelio nebelieka



#4.
f4 <- function(a = 1L,b){
  if(class(a)!="numeric"&&class(b)!="numeric"){
    stop("a+b neapibrezta")
  }
  a+b
}
#L siuo atveju nurodo, kad mes turesime integer. R numatytai skaicius (net ir pvz 4 (be kablelio) ima kaip double)

f4('a','b')
test<-f4(b=6)#jau nebegauname L, nes b pas mus ne integer
typeof(test)

test2<-f4(2,6)
test2