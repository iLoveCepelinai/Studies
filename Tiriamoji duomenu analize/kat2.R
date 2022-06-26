install.packages("RColorBrewer")
library("RColorBrewer")

duom<-read.csv("C:/Users/Acer/Documents/covid.txt");head(duom)
duomenys<-subset(duom,month==5&day==10,select=c(cases,continentExp))
names(duomenys)<-c("susirgimai", "zemynas")
duomenys<-duomenys[complete.cases(duomenys), ];head(duomenys)
filter<- duomenys$susirgimai>0
duomenys<-duomenys[filter,]; head(duomenys)
                      #kazkodel visur yra zemynas "Other" nors duomenu su juo nera
#lygiai                           
levels(duomenys$zemynas)
#dazniu lentele
m<-table(duomenys$zemynas); m
#sukauptuju dazniu lentele
mm<-cumsum(m);mm
#moda (nurodo jo pavadinima ir vieta lenteleje)
which(m==max(m))
#santykiniu dazniu lentele
n<-length(duomenys$zemynas)
p<-m/n; p
#sukauptuju santykiniu dazniu lentele
pp<-cumsum(p);pp
names(pp)
#dazniu daugiakampis
plot(m,type='l',lwd=3, main="zemynai", ylab = "kiekis", col=2); grid() 
#sukauptuju dazniu daugiakampis                       neduoda zemynu pavadinimu!!!
plot(mm,type='l',lwd=3, main="zemynai", ylab = "procentai",xlab="zemynai", col=3); grid() 
#santykiniu dazniu daugiakampis
plot(p,type='l',lwd=3, main="zemynai", ylab = "kiekis", col=2); grid() 
#santykiniu sukauptuju dazniu daugiakampis            neduoda zemynu pavadinimu!!!
plot(pp,type='l',lwd=3, main="zemynai", ylab = "procentai", xlab="zemynai",col=3); grid() 
#stulpeliu diagrama
vv<-barplot(m, main = "zemynai", col = brewer.pal(n = 5, name = "RdBu"))
lines(vv,m,lwd=2)
#skrituline diagrama
names(m)<-paste(names(m),' (',round(prop.table(m)*100),'%)',sep='')
pie(m, col=brewer.pal(n = 5, name = "RdBu"), main = "zemynai")
