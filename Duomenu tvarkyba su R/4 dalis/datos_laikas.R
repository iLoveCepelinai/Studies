library(tidyverse)
library(stringr)
library(lubridate)
library(nycflights13)

today()
now()

#is stringu
ymd("2017-01-31")

mdy("January 31st, 2017")

dmy("31-Jan-2017")

#is int
ymd(20170131)


ymd_hms("2017-01-31 20:11:59")
mdy_hm("01/31/2017 08:01")

#nustatoma laiko zona paties
ymd(20170131, tz = "UTC")

#PVZ
flights_dt <- flights %>% 
  filter(!is.na(dep_time), !is.na(arr_time)) %>% 
  mutate(
    dep_time = make_datetime_100(year, month, day, dep_time),
    arr_time = make_datetime_100(year, month, day, arr_time),
    sched_dep_time = make_datetime_100(year, month, day, sched_dep_time),
    sched_arr_time = make_datetime_100(year, month, day, sched_arr_time)
  ) %>% 
  select(origin, dest, ends_with("delay"), ends_with("time"))


flights %>% 
  select(year, month, day, hour, minute) %>% 
  mutate(departure = make_datetime(year, month, day, hour, minute))


?wday #rodo savaites diena (kartais savaites pradzia skaiciuojama nuo skemadienio)

datetime <- ymd_hms("2016-07-08 12:34:56")
year(datetime)
month(datetime)
mday(datetime)
yday(datetime)
wday(datetime)
wday(datetime, label = TRUE)
wday(datetime, label = TRUE, abbr = F)


#datu apvalinimas
flights_dt %>% 
  count(week = floor_date(dep_time, "week")) %>% 
  ggplot(aes(week, n)) +
  geom_line()


#LAIKU SKIRTUMAI
h_age <- today() - ymd(19791014)
h_age
as.duration(h_age)

dseconds(15)
dminutes(10)
dhours(c(12, 24))
ddays(0:5)
dweeks(3)
dyears(1)

#galimi artimetiniai veiksmai
dyears(1) + dweeks(12) + dhours(15)


one_pm <- ymd_hms("2016-03-12 13:00:00", tz = "America/New_York")
one_pm
one_pm + ddays(1)#padideja viena h del sukamo laiko. Netiketa negerai

#PERIODAI
#tan naudoti days, ten prideda zmogiskai ir mums suprantamai, ne grieztai sekundemis
one_pm + days(1)


#INTERVALAI
years(1) / days(1)
#hmmm sus, nes egzistuoja keliamieji metai kause

next_year <- today() + years(1)
(today() %--% next_year) / ddays(1)#sukuriame intervalo tipo objekta



#LAIKO JUOSTOS
Sys.timezone()

length(OlsonNames())#daug laiko juostu
head(OlsonNames())


(x1 <- ymd_hms("2015-06-01 12:00:00", tz = "America/New_York"))
(x2 <- ymd_hms("2015-06-01 18:00:00", tz = "Europe/Copenhagen"))
(x3 <- ymd_hms("2015-06-02 04:00:00", tz = "Pacific/Auckland"))

x1 - x2 #keista, skirtumas 0

x4 <- c(x1, x2, x3)
x4

x4a <- with_tz(x4, tzone = "Australia/Lord_Howe")
x4a
x4a - x4#vel visur 0. Keiciasi tik atvaizdavimas

#Todel gali naudoti
x4b <- force_tz(x4, tzone = "Australia/Lord_Howe")
x4b
x4b - x4 #randami skirtumai



################################
#Pratybos
################################

#1.
#Susigeneruokite dataframe, kuriame butu metu, menesio ir dienos stulpeliai, kurie visi butu
#numeric tipo. Is gautu reiksmiu sukurkite datu stulpeli. Kurdami data laiko juosta parinkite
#atsitiktinai is DB OlsonNames.

getTime <- function(y,m,d,tz){
  ymd(paste(y,m,d,sep="-"), tz=tz)
}

n <-100 
df <- data.frame(y=sample(c(2000:2021), size = n, replace = T),
                m=sample(c(1:12), size = n, replace = T),
                d=sample(c(1:28), size = n, replace = T),
                tz=OlsonNames()[sample(c(1:length(OlsonNames())), size = n, replace = T)])%>%
  mutate(ddata = sapply(X=c(1:n), FUN = function(i){getTime(df$y[i],df$m[i],df$d[i])}))

#2.
#Paimkite 3 laisku antrastes is pasto dezutes. Isfiltruokite datas panaudodami reguliarias 
#israikskas ir is gautu teksto fragmentu sukurkite datu vektoriu.

antrastes<-c('nuo:	IKI - Mes visi mylim maistą! <noreply@iki.lt>
           kam:	matasimas@gmail.com
         data:	2021-12-15 10:03
         tema:	IKI gimtadienis! Užbaikite registraciją ir dalyvaukite žaidime
         siųsta iš:	iki.lt
         sauga:	 Įprasta šifruotė (TLS) Sužinokite daugiau',
             
         'nuo:	Autoplius.lt <no-reply@autoplius.lt>
           atsakyti kam:	"no-reply@autoplius.lt" <no-reply@autoplius.lt>
           kam:	matasimas@gmail.com
         data:	2021-12-15 07:09
         tema:	Ratlankiai - 1 naujas skelbimas
         siųsta iš:	autoplius.lt
         pasirašė:	autoplius.lt
         sauga:	 Įprasta šifruotė (TLS) Sužinokite daugiau',
         
         'nuo:	EOLTAS <eparduotuve@eoltas.lt> per soundest.email 
         kam:	matasimas@gmail.com
         data:	2021-12-13 15:05
         tema:	DOVANŲ idėjos nuo EOLTAS komandos! 🎁
         siųsta iš:	soundest.email
         pasirašė:	soundest.email
         sauga:	 Įprasta šifruotė (TLS) Sužinokite daugiau')

datos_str <- str_extract_all(antrastes, pattern = "\\d{4}-\\d{2}-\\d{2}\\s\\d{2}:\\d{2}",
                             simplify = T) #simplify pavercia is saraso i char
datos_str
datos_gal <- ymd_hm(datos_str)
datos_gal

wday(datos_gal, week_start = 1)

#3.
#Parasykite funkcija, kuri imtu argumenta metai ir atspausdintu visu tu metu pirmadieniu
#datas. Kaip apibendrinti bet kokiai savaites dienai??????

month(today())
week(today())
wday(today(), week_start = 1) #nes mes ne dunduliai amerikieciai



wday(c(today(), "2021-08-12"), week_start = 1) #veikia su vektoriumi

ymd(paste("2000", "01", "01", sep="-"))+ddays(5)

y<-2021
getMondays <- function(y) {
  metuDatos <- as_date(sapply(X = c(0:364), FUN = function(i){
    as.character(ymd(paste(as.character(y), "01", "01", sep="-"))+ddays(i))
  }))
  metuDatos[wday(metuDatos, week_start = 1) == 1]
}

getMondays(2021)

#apibendrinta versija (pridedame wd - savaites diena, kurios datos norime)
getMondays <- function(y, wd) {
  metuDatos <- as_date(sapply(X = c(0:364), FUN = function(i){
    as.character(ymd(paste(as.character(y), "01", "01", sep="-"))+ddays(i))
  }))
  metuDatos[wday(metuDatos, week_start = 1) == wd]
}

getMondays(y = 2021, wd = 4)


#4.
#Parasykite funkcija, kuri imtu 2 argumentus: data1 ir trukme1 bei grazintu:
# a)intervala (data1+/- trukme1)
# b)metu diena, atitinkancia data data1-2*trukme1;
# c)menesi, kuris "artimesnis" faktiniam data1 menesiui (pvz.: jei data1=2002-01-05, tai yra
#   2 gretimi menesiai. Faktinis data1 menuo yra 01. Gretinimi menesiai butu 2001 12 ir 2002
#   02 --> fja turetu grazinti siai datai skaiciu 12).
# d) a dalies intervalo ilgi minutemis.

#Tarkime trukme1 yra specifikuojama sekundemis.

#a)
f4a <- function(data1, trukme1){
  ((data1-trukme1)%--%(data1+trukme1))
}

f4a(data1 = datos_gal[1], trukme1 = duration(3600))


#b)
f4b <- function(data1, trukme1){
  data1-2*trukme1
}

f4b(data1 = datos_gal[1], trukme1 = duration(3600))
datos_gal[1] #skirtumos 2h, tai veikia


#c)
f4c <- function(data1){
  fakt_men <- month(data1)
  if (fakt_men == 1){
    kaire_data <- ymd(paste(year(data1)-1,12,31, sep = "-"))
    desine_data <- ymd(paste(year(data1),2,1, sep = "-"))
  }
  else {if (fakt_men == 12){
    kaire_data <- ymd(paste(year(data1),11,30, sep = "-"))
    desine_data <- ymd(paste(year(data1)+1,1,1, sep = "-"))
  }
    else{
      kaire_data <- data1 - ddays(mday(data1))
      desine_data <- ymd(paste(year(data1),fakt_men+1,1, sep = "-"))
    }
  }
  if(data1-kaire_data > desine_data-data1){month(desine_data)}
  else{
    if(data1-kaire_data<desine_data-data1){month(kaire_data)}
    else{c(month(kaire_data),month(desine_data))
    }
  }
}

data1 <- ymd("2002-02-12")
data2 <- ymd("2002-01-14")
data3 <- ymd("2002-12-18")

f4c(data1 = data3)


#d)
f4d <- function(data1, trukme1){
  f4a(data1, trukme1)/dminutes(1)
}

f4d(data1 = datos_gal[1], trukme1 = duration(120))
