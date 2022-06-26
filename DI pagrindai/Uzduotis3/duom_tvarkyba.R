library(readr)
library(dplyr)
#nuskaitome duomenis
iris_Copy <- read_csv("iris - Copy.csv", col_names = FALSE)
#priskiriame laukeliams id
iris_Copy$id <- 1:nrow(iris_Copy)

#atrenkame tik setosa geles
setosa <- iris_Copy %>% filter(X5 == "Iris-setosa")
#sukuriame setosu mok_test ir new aibes
trte_setosa <- setosa %>% dplyr::sample_frac(.80)
test_setosa  <- dplyr::anti_join(setosa, trte_setosa, by = 'id')


#atrenkame tik setosa geles
versicolor <- iris_Copy %>% filter(X5 == "Iris-versicolor")
#sukuriame setosu mok_test ir new aibes
trte_versicolor <- versicolor %>% dplyr::sample_frac(.80)
test_versicolor <- dplyr::anti_join(versicolor, trte_versicolor, by = 'id')


#atrenkame tik setosa geles
virginica <- iris_Copy %>% filter(X5 == "Iris-virginica")
#sukuriame setosu mok_test ir new aibes
trte_virginica <- virginica %>% dplyr::sample_frac(.80)
test_virginica <- dplyr::anti_join(virginica, trte_virginica, by = 'id')


#sujungiame train test (trte) i viena faila, test i kita
trte <- rbind(trte_setosa, trte_versicolor, trte_virginica)
test <- rbind(test_setosa, test_versicolor, test_virginica)
#ismetame nebereikalinga id stulpeli
trte <- trte[-ncol(trte)]
test <- test[-ncol(test)]

#issaugome i naujus duomenu failus
write.table(trte,"iris_train_test.csv", quote = F, sep = ",", row.names = F, col.names = F)
write.table(test,"iris_new.csv", quote = F, sep = ",", row.names = F, col.names = F)
