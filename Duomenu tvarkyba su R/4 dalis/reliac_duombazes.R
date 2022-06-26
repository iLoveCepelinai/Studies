library(tidyverse)#siaip cia naudosime dplyr bible
library(nycflights13)
#Geriausia ziurek knyga
#https://r4ds.had.co.nz/relational-data.html

#rasyti sitaip kad gautum lenteles
#nycflights13::

inner_join(flights, weather, by = c("year", "month", "day"))

#jei pavadinimai skiriasi lentelese
x <- tribble(
  ~key_x1, ~key_x2, ~val_x,
  1, 1, "x1",
  2, 2, "x2",
  3, 3, "x3"
)
y <- tribble(
  ~key_y1, ~key_y2, ~val_y1, ~val_y2,
  1, 1,"y1", "y11",
  2, 2,"y2", "y22",
  4, 3,"y3", "y33"
)

#mutating join, nes prie x lenteles prideda y stulpelius
left_join(x,y,by=c("key_x1" = "key_y1", "key_x2" = "key_y2")) %>%
  select(key_x1, val_y1)

#sitas neprideda jokiu nauju stulpeliu
semi_join(x,y,by=c("key_x1" = "key_y1", "key_x2" = "key_y2"))


# Svarbu, galoi buti kontras is sito
# http://klevas.mif.vu.lt/~visk/Duomenu_tvarkyba_R/NorthWind/

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

# Kuris darbuotojas 2002 metais aptarnavo daugiausiai uzsakymu. Atspausdinti jo varda ir
# pavarde.
# Paziureti pazydzius kontrolinio