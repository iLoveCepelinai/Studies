library(tidyverse)

x1 <- c("Dec", "Apr", "Jan", "Mar")

#Pirma susikuri galimu variantu vektoriu
month_levels <- c(
  "Jan", "Feb", "Mar", "Apr", "May", "Jun", 
  "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
)

y1 <- factor(x1, levels = month_levels)
y1

#dabar sort veikia kaip mes norime (pagal musu eiliskuma)
sort(y1)

#galime tureti nes skaicius (svarbu, kad po faktoriumi slepiasi skaicius)
as.numeric(y1)



#Pvz su klaida
x2 <- c("Dec", "Apr", "Jam", "Mar")
y2 <- factor(x2, levels = month_levels)
y2
#NA reiksme vietoje zodzio su klaida
sort(y2)

#jei nori kad mestu klaida
y2 <- parse_factor(x2, levels = month_levels)

#jein nori issaugoti eiliskuma kaip paduotu duomenu
f2 <- x1 %>% factor() %>% fct_inorder()
f2
#lygiai
levels(f2)


gss_cat


relig_summary <- gss_cat %>%
  group_by(relig) %>%
  summarise(
    age = mean(age, na.rm = TRUE),
    tvhours = mean(tvhours, na.rm = TRUE),
    n = n()
  )
#> `summarise()` ungrouping output (override with `.groups` argument)

ggplot(relig_summary, aes(tvhours, relig)) + geom_point()


ggplot(relig_summary, aes(tvhours, fct_reorder(relig, tvhours))) +
  geom_point()


gss_cat %>%
  mutate(marital = marital %>% fct_infreq() %>% fct_rev()) %>%
  ggplot(aes(marital)) +
  geom_bar()



#Perkuodojami faltoriai
gss_cat %>%
  mutate(partyid = fct_recode(partyid,
                              "Republican, strong"    = "Strong republican",
                              "Republican, weak"      = "Not str republican",
                              "Independent, near rep" = "Ind,near rep",
                              "Independent, near dem" = "Ind,near dem",
                              "Democrat, weak"        = "Not str democrat",
                              "Democrat, strong"      = "Strong democrat",
                              "Other"                 = "No answer",
                              "Other"                 = "Don't know",
                              "Other"                 = "Other party"
  )) %>%
  count(partyid)

#Sujungia mazesnes grupes (n - kiek grupiu norime)
gss_cat %>%
  mutate(relig = fct_lump(relig, n = 10)) %>%
  count(relig, sort = TRUE) %>%
  print(n = Inf)



########################
#PRATYBOS
########################
#1.
#Isrinkite atsitiktikne imti (1000 eil.) is rinkinio fruits.
#1.1. Pavertskite gauta vektoriu faktroriumi. levels argumentui naudokite visu isrinktu vaisiu
# pavadinimus + kategorija "others".

vaisiai <- sample(x = fruit, 1000, replace = T)
zymes <- c(unique(vaisiai), "others")
zymes
sort(zymes)

vaisiai <- factor(x=vaisiai, levels = sort(zymes))
levels(vaisiai)


#1.2 Sukurkite kita faktoriu (is jau sukurto), kurio kategorijos butu rikiuojamos:
# a) pagal daznuma
# b) pagal reiksmiu pasiteikyma eiluteje
# c) pagal raidziu skaiciu pavadinime

#a1)
vaisiai_a1 <- fct_infreq(vaisiai) #turetu surikiuoti pagal daznuma (nuo gausiausios)
levels(vaisiai_a1)

vaisiai_a1 %>% as_tibble() %>% group_by(value) %>% count(sort = T)
vaisiai %>% as_tibble() %>% group_by(value) %>% count(sort = T)

#a2)nuo maziausio iki didziausio
vaisiai_a2 <- fct_rev(fct_infreq(vaisiai)) #turetu surikiuoti pagal daznuma (nuo maziauisia)
levels(vaisiai_a2)

vaisiai_a2 %>% as_tibble() %>% group_by(value) %>% count()
vaisiai %>% as_tibble() %>% group_by(value) %>% count(sort = T)


#b)
#vaisiai_b <- fct_inorder(factor(fruit(factor[unclass(vaisiai)))
vaisiai_b <- fct_inorder(as.vector(vaisiai))
levels(vaisiai_b)[1:10]
vaisiai[1:10]


#c)
vaisiaiDF <- as_tibble(x=vaisiai) %>% rename(v = value) %>%
  mutate(simb_sk = str_length(v)) %>%
  mutate(v = fct_reorder(v, simb_sk, min)) #del sito zr. zemiau

levels(vaisiaiDF$v)

?fct_reorder
#is help
df <- tibble::tribble(
  ~color,     ~a, ~b,
  "blue",      1,  2,
  "blue",      5,  2,
  "green",     6,  2,
  "purple",    3.1,  3,
  "red",       2,  3,
  "yellow",    5,  1
)
df$color <- factor(df$color)
fct_reorder(df$color, df$a, mean) #pagal vidurki a stulpelio priskiria faktoriau lygius
#matome, kad red vidurkis maziausias, del to pirma. Vietoje mean gali bet kokia fja rasyti

#antras budas
fct_reorder(vaisiai, str_length(vaisiai), min)


#2
#Paimkite rinkini mtcars. Paverskite stulpelis gear faktoriumi ir isrikiuokite  jo kategorijas
#panaudodami stulpeliu wt (weight) 10% nupjauta vidurki, padalinta is standartinio nuoktypio.
mtcars

df <- mtcars %>%
  mutate(gear = fct_reorder(factor(df$gear), df$wt, function(w){mean(w, trim = 0.1,
  na.rm = T)/sd(w, na.rm = T)}))

fct_reorder(factor(df$gear), df$wt, function(w){mean(w, trim = 0.1, na.rm = T)/
    sd(w, na.rm = T)})

