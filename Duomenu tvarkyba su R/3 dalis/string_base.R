library(tidyverse)#naudosime stringr paketa kuris yra tidyverse

stringas <- "Labas \"kabutese\""

stringas#nemeta error bet ne taip kaip norime
writeLines(stringas)#wow geras veikia omg (jei nori kad veiktu \n irgi sito reikia)

#help
?"'"

#stringu kombinavimas
str_c("x", "y", "z")
str_c("prefix-", c("a", "b", "c"), "-suffix")
str_c(c("x", "y", "z"), collapse = ", ")

#panasiai kaip su paste funkcija
name <- "Hadley"
time_of_day <- "morning"
birthday <- FALSE

str_c(
  "Good ", time_of_day, " ", name,
  if (birthday) " and HAPPY BIRTHDAY",
  ".")


#str_sub
x <- c("Apple", "Banana", "Pear")
str_sub(x, 1, 3)
str_sub(x, -3, -1)


#rikiavimas pagal abecele
x <- c("apple", "eggplant", "banana")
str_sort(x, locale = "en")  # English