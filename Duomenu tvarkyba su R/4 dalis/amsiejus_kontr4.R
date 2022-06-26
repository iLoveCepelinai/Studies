#Matas Amsiejus, DM2
#4 kontrolinis

library(tidyverse)
library(lubridate)
library(nycflights13)


########################
# 1 uzduotis
########################
# a)
dt1 <- flights %>% select(time_hour, dep_delay) %>%
  mutate(datos = ymd_hms(time_hour)) %>%
  mutate(sav_diena = wday(datos, week_start = 1)) %>%
  select(sav_diena, dep_delay) %>%
  mutate(sav_diena = as.character(sav_diena)) %>%
  mutate(sav_diena = fct_recode(sav_diena,
                               pirm = "1",
                               antr = "2",
                               trec = "3",
                               ketv = "4",
                               penkt ="5",
                               sest = "6",
                               sekm = "7"))
# b)
angliskai_n <- dt1$sav_diena %>% fct_shift(n=-1)
levels(angliskai_n)


# c1)
dvi_kat <- dt1$sav_diena %>%
  fct_collapse(
    siokiadienis = c("pirm","antr","trec","ketv","penkt"),
    savaitgalis = c("sest","sekm"))
levels(dvi_kat)


# c2)
pop_dienos <- dt1$sav_diena %>%
  fct_lump(n = 3,other_level = "kitos sav. dienos")
levels(pop_dienos)


# d)
#Man neveike kreipimasis i funkcija, todel tiesiog iklijavau visa funkcija
vid_maks10 <- function(delay){
  sorted <- sort(delay, decreasing = T)[1:10]
  mean(sorted, na.rm = T)
}

test <- fct_reorder(dt1$sav_diena, .x = dt1$dep_delay, function(delay){
  sorted <- sort(delay, decreasing = T)[1:10]
  mean(sorted, na.rm = T)
})

levels(test)


########################
# 2 uzduotis
########################

intervalas <- function(dat, trukme){
  ((dat-trukme)%--%(dat+trukme))
}
today()
intervalas(today(), duration(3600))
#Gauname gerai, nes today pagal nutylejima yra lygiai vidurnaktis dabartines datos


########################
# 3 uzduotis
########################


########################
#Nuskaitymas is interneto:
########################
path <- "http://klevas.mif.vu.lt/~visk/Duomenu_tvarkyba_R/NorthWind/"

categories <- read.csv2(file = paste0(path,"Categories.txt"),header = T,sep = ";",dec = ",")
customers <- read.csv2(file = paste0(path,"Customers.txt"),header = T,sep = ";",dec = ",")
employees <- read.csv2(file = paste0(path,"Employees.txt"),header = T,sep = ";",dec = ",")
order_details <- read.csv2(file = paste0(path,"Order_Details.txt"),header = T,sep = ";",
                           dec = ",")
orders <- read.csv2(file = paste0(path,"Orders.txt"),header = T,sep = ";",dec = ",")
products <- read.csv2(file = paste0(path,"Products.txt"),header = T,sep = ";",dec = ",")
shippers <- read.csv2(file = paste0(path,"Shippers.txt"),header = T,sep = ";",dec = ",")
suppliers <- read.csv2(file = paste0(path,"Suppliers.txt"),header = T,sep = ";",dec = ",")

orders <- orders %>% mutate(OrderDate = ymd_hms(orders$OrderDate),
                            RequiredDate = ymd_hms(orders$RequiredDate),
                            ShippedDate = ymd_hms(orders$ShippedDate))




########################
# a)
nepop_preke <- left_join(orders,order_details) %>% filter(year(OrderDate)==1997) %>%
  left_join(y=customers) %>% filter(Country == "Germany") %>%
  left_join(y=products, by = "ProductID") %>%
  left_join(categories) %>% group_by(CategoryName) %>% summarise(pop_prek = n()) %>%
  slice_min(order_by = pop_prek, n=1)

#Kur rasau by=ProductID, nes R kazkodel dar jungia ir pagal unitPrice
nepop_preke

# b)
nuolaidos <- left_join(orders, order_details) %>% left_join(y=products, by = "ProductID") %>%
  left_join(y=categories) %>% mutate(metai = year(OrderDate)) %>%
  group_by(CategoryName, metai) %>%
  summarise(vid_nuolaida = mean(Discount),
            max_nuolaida = max(Discount))

########################
# 4 uzduotis
########################
tiekejai <- left_join(orders, shippers, by = c("ShipVia" = "ShipperID")) %>%
  rename(ShipCompName = CompanyName) %>%
  left_join(y=order_details) %>%
  left_join(y=products, by = "ProductID") %>% left_join(y=suppliers, by = "SupplierID") %>%
  select(CompanyName, ProductName, ShipCompName) %>%
  group_by(CompanyName, ProductName) %>%
  mutate(Prekes_pop = n()) %>%
  ungroup() %>%
  group_by(CompanyName) %>%
  slice_max(order_by = Prekes_pop, n = 1) %>%
  group_by(CompanyName, ShipCompName) %>%
  mutate(kurjerioPop = n()) %>% unique()



#Na bent atsakymas tas pats, yay :)
nepop_preke <- left_join(orders,order_details) %>% filter(year(OrderDate)==1997) %>%
  right_join(y=customers) %>% filter(Country == "Germany") %>%
  right_join(y=products, by = "ProductID") %>%
  left_join(categories) %>% group_by(CategoryName) %>% summarise(pop_prek = n()) %>%
  slice_min(order_by = pop_prek, n=1)