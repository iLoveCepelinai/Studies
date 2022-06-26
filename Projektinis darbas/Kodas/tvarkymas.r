library(data.table)
library(lubridate)
library(tidyverse)

orai<-as.data.frame(fread("orai.csv"))
irradiation<-as.data.frame(fread("Plant irradiation.csv"))
energy_full<-as.data.frame(fread("Total energy export.csv"))
IR<-as.data.frame(fread("IR sensor.csv"))
ilgis<-as.data.frame(fread("dienos_ilgis.csv", header=FALSE))

# Energy
energy<-energy_full[,c(3, 10, 11)]
x<-ymd_hms(energy$timestamp)
energy$metai<-year(x)
energy$menuo<-month(x)
energy$diena<-day(x)

df<-energy
df$IR<-IR$avg_val
df$irr<-irradiation$avg_val
names(df)[1] <-"energy"

df<-subset(df, (metai!=2019 | menuo !=9 | diena!=6))
df<-subset(df, (metai!=2021 | menuo !=3 | diena!=22))


df<-df[,c(1,7,8,3,4,5,6)]

df$energy<-as.numeric(gsub(",", ".", df$energy))
df$IR<-as.numeric(gsub(",", ".", df$IR))
df$irr<-as.numeric(gsub(",", ".", df$irr))

df$data<-make_date(df$metai, df$menuo, df$diena)

# Sugrupuojame duomenis dienomis
df1<-df %>%
  group_by(data) %>%
  summarise(sum_irr=sum(irr), sum_IR=sum(IR), 
            sum_e=sum(energy), vid_irr=mean(irr), 
            vid_IR=mean(IR), vid_e=mean(energy))


df1 %>% filter(year(data)==2019) %>% summary()

# Rankiniu budu pridedame praleista diena
ilgis <- rbind(ilgis[1:481,],c("12/31/2020",	"7h 4m 55s"),ilgis[-(1:481),])

orai<-orai[,-c(6,7,11)]

# Praleistos reiskmes
sum(is.na(orai))

# Vidutine temperatura (gauta is wunderground.com)
sum(is.na(orai$tavg))
orai$tavg[which(is.na(orai$tavg))]<-0.7

# Krituliai
sum(is.na(orai$prcp))
orai$prcp[which(is.na(orai$prcp))]<-0

# Vejo greitis
sum(is.na(orai$wspd))
orai$wspd[which(is.na(orai$wspd))]<-25.7

# Slegis
sum(is.na(orai$pres))
orai$pres[which(is.na(orai$pres))]<-1002.79

# Sujungiame orus ir elektriniu duomenis
df2<-as.data.frame(c(df1, orai))

# Prijungiame dienos sviesos ilgi (valandomis)
df2$d_ilg<-as.numeric(seconds(hms(ilgis$V2)))/3600

# Atstatome cumsum (imsime suminius dienos duomenis, ne vidutinius)
df2$irr<-diff(c(0,df2$sum_irr))
df2$e<-diff(c(0, df2$sum_e))

df2$date<-as.character(df2$date)

# del laiko persukimo
df3<-df2[df2$date!='2019-10-27',]
df3<-df3[df3$date!='2019-10-28',]
df3<-df3[df3$date!='2020-10-25',]
df3<-df3[df3$date!='2020-10-26',]
# klaidos
df3<-df3[df3$date!='2020-05-29',]
df3<-df3[df3$date!='2020-05-30',]
df3<-df3[df3$date!='2020-06-01',]
df3<-df3[df3$date!='2020-06-02',]
# neistryneme vasaros laiko ivedimo, gali reiketi patikrinti veliau 
# 2020-03-29

df4<-df3[,c(8, 9, 12, 13, 15:17, 3, 18)]
names(df4)<-c('data', 'temp', 'krituliai', 'v_gr', 'slegis', 'd_ilg', 'irr',
              'IR','kwh')

write.csv(df4, "galDuom.csv", row.names=FALSE)
