library(tidyverse)
library(nycflights13)

datasets::airquality
df<-airquality
attributes(df)
df2<-as_tibble(df)
df2


tibble(
  x = 1:5, 
  y = 1, 
  z = x ^ 2 + y
)


tb <- tibble(
  `:)` = "smile", #stulpelis pavadinimu :)
  ` ` = "space", #-||- tarpu 
  `2000` = "number" #-||- 2000
)
tb

#jei nori stulpelio, tai reikia naudoti "backtickus" - ''
tb$` `

flights

#filtravimas(filter)
#tibble nemodifikuoja pradinio dataframe
filter(flights, month == 1, day == 1)
filter(flights, month == 11 | month == 12)#galima naudoti logika (!=, |(arba),...) (, reiksia 'AND' (&&))
filter(flights, month %in% c(11, 12))


df <- tibble(x = c(1, NA, 3))
filter(df, x > 1)
#po filter automatiskai neliaka NA reiksmiu
filter(df, is.na(x) | x > 1)
#reikia rankiniu budu issaugoti NA jei nori


#rikiavimas (arange)
arrange(flights, year, month, day)#automatiskai didejanciai
arrange(flights, desc(dep_delay))


#select sakinys
select(flights, year, month, day)
select(flights, year:day)#veikia nuo to iki to imtinai
select(flights, -(year:day))#ismeti pasirinktus stulpelius su -
#naudokis helper funkcijomis (pvz "starts_with", ...)

rename(flights, tail_num = tailnum)

select(flights, time_hour, air_time, everything())#patogu persikelti stulpelius i prieki


#Mutate funkcija (skaiciuojami nauji stulpeliai)
flights_sml <- select(flights, 
                      year:day, 
                      ends_with("delay"), 
                      distance, 
                      air_time
)
flights_sml
mutate(flights_sml,
       gain = dep_delay - arr_delay,#naujas stulpelis kaireje pusese = , o desineje stulpeliai, kuriuos naudojame
       speed = distance / air_time * 60
)
#su paprastu r butu flights_sml$gain<-flights_sml$dep_delay - flights_sml$arr_delay

#vel helper funkcijas 5 skyriuje ziureti

#summerise - suvestines
#viena stulpeli rasant nelabai apsimoka, bet kai keli stulpeliai ar reikia skaiciuoti kelis dalykus verta tada
summarise(flights, delay = mean(dep_delay, na.rm = TRUE))

#sugrupuojant skaiciuoja tada vidurki grupems
by_day <- group_by(flights, year, month, day)
summarise(by_day, delay = mean(dep_delay, na.rm = TRUE))



flights %>% filter(!is.na(dep_delay), !is.na(arr_delay)) %>% 
  group_by(year, month, day) %>% 
  summarise(
    avg_delay1 = mean(arr_delay),
    avg_delay2 = mean(arr_delay[arr_delay > 0]) # the average positive delay
  )

not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(
    first = min(dep_time),
    last = max(dep_time)
  )

#counts (n()) - skaiciuoja kiek grupeje
flights %>% filter(!is.na(dep_delay), !is.na(arr_delay)) %>% 
  group_by(dest) %>% 
  summarise(carriers = n_distinct(carrier)) %>% #skaiciuoja kiek unikaliu grupeje
  arrange(desc(carriers))

###################################

#I kuri oro uosta buvo dazniausiai skrendama 5 menesi, kai apsiribojame tik arr_delay laikais. Pateikti ju 
#top 3. Patgu naudotis count ir head fjom
df <- flights
df %>%
  filter(month==5, arr_delay<0) %>%
  count(dest) %>% #suskaiciuoja kiek dest unikalus kartojasi
  arrange(desc(n)) %>% #arba arrange(n) 
  head(3) #tail(3)

#kiekvienam menesiui nurodykite po 5 skrydzius, kuriu metu pasiekiamas didziausias vid greitis. Nurodymas:
#patogu pasinaudoti slice seimos funkcijomis

#sad nera jokio vidutinio greicio, reikia suskaiciuoti (s/t)
#a)
df %>%
  mutate(avgSpeed=distance/air_time)%>%#sukuriame nauja stulpeli
  group_by(month) %>%#toliau visos operacijos bus atliekamos kiekvienam menesiui
  arrange(desc(avgSpeed)) %>%
  head(5)#nelabai veikia, matyt nes grupes
#b)
df %>%
  mutate(avgSpeed=distance/air_time)%>%#sukuriame nauja stulpeli
  group_by(month) %>%#toliau visos operacijos bus atliekamos kiekvienam menesiui
  arrange(desc(avgSpeed)) %>%
  slice(c(1:5)) %>%
  select(month, avgSpeed)
#c)
df %>%
  mutate(avgSpeed=distance/air_time)%>%#sukuriame nauja stulpeli
  group_by(month) %>%#toliau visos operacijos bus atliekamos kiekvienam menesiui
  slice_max(avgSpeed, n=5) %>%
  select(month, avgSpeed)


#Kiek yra stulpeliu, kuriu pavadinimai baigiasi balse (a,e,i,o,u,y)? Patogu naudoti ends_with
balses <- c('a','e','i','o','u','y')
df %>%
  select(ends_with(balses)) %>%
  length()

#atrinkti tuos stulpeliu pavadinimus, kuruose nera tusciu reiksmiu. panaudoti select_if
df %>%
  select_if(.predicate = function(x){sum(is.na(x))}==0)%>%
  names()


#sukurti nauja kintamaji, rodanti skrydzio laika valandomis (airtime/60); panaudoadamo si kintamaji 
#sukurti kintamaji flight igijanti 2 reiksmes: 0, kai skydzio trukme valnadomis nevirsijo 3, 1 - kai virsijo

df %>%
  mutate(skr_trukme_val=air_time/60,
         flight_dur=ifelse(skr_trukme_val>3,1,0)) %>%
  select(skr_trukme_val, flight_dur)


f <- function(x){
  ifelse(x>3,1,0)
}
df %>%
  mutate(skr_trukme_val=air_time/60,
         flight_dur=f(skr_trukme_val)) %>%
  select(skr_trukme_val, flight_dur)

#sukurti nauja kintamaji, rodanti skrydzio laika valandomis (airtime/60); panaudoadamo si kintamaji 
#sukurti kintamaji flight igijanti 3 reiksmes: 0, 1 kai skydzio trukme valnadomis >3 nevirsijo 5,
#2 - kai virsijo 5

df %>%
  mutate(skr_trukme_val=air_time/60,
         flight_dur=ifelse(skr_trukme_val>3,ifelse(skr_trukme_val>5,2,1),0)) %>%
  select(skr_trukme_val, flight_dur)

#sukurkite nauja kintamaji kiekvienam menesiui rodanti kiek skrydziu lektuvas atliko per menesi. Paeme 
#konkretu lektuva ir menesi nufiltruokite jo atliktus skrydzius ir tiesiogiai patikrinkkite savo skaiciavims

df %>%
  group_by(month, tailnum) %>%
  mutate(skr_sk=n()) %>% #cia kiekvienai eilutei sukuria
  select(month, tailnum, skr_sk)%>%
  unique()


#patikrinti
df %>%
  filter(month==1, tailnum == "N14228")
#matome, kad yra 15 eiluciu, o pirma menesi irgi rodo kad tiek skrydziu atliko, tai veikia

#agregavimas panaudojant funkcija
#kiekvieno menesio skrydziu trukmiu dispersija, vidurki bei kvadratu viurrki (ji apskaiciuoti paciu parasyta fja)

ss<-function(x){
  y<-x[!is.na(x)]
  mean(y**2)
}

df %>%
  group_by(month) %>%
  summarise(truk_vid=mean(air_time, na.rm = T),
            truk_disp=var(air_time, na.rm = T),
            kv_vid = ss(air_time))

#agregavimas panaudojant vartotojo sukuta kintamaji
#bendra visu lektuvu nuskrista atstuma kiekvienam ketvirciui
q_dict <- sort(c(rep(1:4, 3)))

q_dict[c(1,1,5,8,9,12)]

df %>%
  mutate(ketvirtis = q_dict[month]) %>%
  group_by(ketvirtis) %>%
  summarise(suminis_atstum = sum(distance, na.rm = T))

#agregavimas, panaudojant filtravima
#bendra kiekviena menesi i oro uostus ORD, ATL, LAX ivykdytu skrydziu skaiciu, isrusiuokite gauta rez
#pagal sumini skrydziu skaiciu mazejimo tvarka

dest_vec<-c("ORD", "ATL", "LAX")
df%>%
  filter(dest %in% dest_vec) %>%
  group_by(month) %>%
  summarise(bendras_skr_sk = n()) %>%
  arrange(desc(bendras_skr_sk))%>%
  select(month, bendras_skr_sk)