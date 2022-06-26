library(dplyr)
library(stats)
library(ggplot2)
library(ggfortify)
# install.packages("devtools")
#devtools::install_github("jlmelville/mnist")
library(mnist)

mnist <- download_mnist()

set.seed(67)
data <- mnist %>% group_by(Label) %>% sample_n(size = 200)

###########################
# K-MEANS
###########################

# Duomenys be labels
data1 <- data[,-785]

## T-SNE dimensijos mazinimas (reikes tolimesniame darbe)

library(Rtsne)

# sutvarkau duomenis
labels<-data$Label
data$Label<-as.factor(data$Label)

colors = rainbow(length(unique(data$Label)))
names(colors) = unique(data$Label)

# T-SNE (2D)
set.seed(67)
tsne <- Rtsne(data1, dims = 2, perplexity=30, verbose=TRUE, max_iter = 500)

# T-SNE vizualizacija
plot(tsne$Y, t='n', main="tsne")
text(tsne$Y, labels=data$Label, col=colors[data$Label])

data_1<- as.data.frame(tsne$Y)

data_1$Label <- data$Label


############### KLASTERIU SKAICIAI ##################

########## empyrinis metodas

empyrinis <- function(duom) {
  klast_sk <- sqrt(length(duom)/2)
  return(klast_sk)
}

# Klasteriu ne daugiau nei 19
empyrinis(data)

######### Elbow

# load required packages
library(factoextra)
library(NbClust)

# Elbow klasteriu parinkimo
fviz_nbclust(data, kmeans, method = "wss", k.max = 15)


# kitas (destytojos) variantas (leisti nebutina)
wcss = vector()
for (i in 1:10) wcss[i] = sum(kmeans(data, i)$withinss)
plot(1:10,
     wcss,
     type = 'b',
     main = paste(' Elbow metodas'),
     xlab = 'Klasteriu skaicius',
     ylab = 'WCSS')

# Panasu, kad nematome linkio tasko


######### silhouette method

fviz_nbclust(data, kmeans, method = "silhouette", k.max = 15) +
  labs(subtitle = "Silhouette method")

# 2,3 arba 11/13

######### KLASTERIZAVIMAS ############

# Imkime 11 teigiant, kad tikrai negali buti 2 klasteriai
set.seed(67)
km.res <- kmeans(x = data1, centers = 11)
y_kmeans = km.res$cluster
#km.res
y_kmeans


# Vizualizacija taskine diagrama (pagal PCA)
#fviz_cluster(km.res, data1, stand = FALSE, geom = 'point')

# Vizualizacija taskine diagrama (pagal t-SNE)
data_tsne <- data_1[,-3]
data_tsne$fill <- rep(0, nrow(data_tsne))

a<-matrix(unlist(data_tsne), nrow = 2000, ncol = 3)

fviz_cluster(list(data = a, cluster = y_kmeans), stand = F , geom = "point")


# heatmapas
data_c <- data1
data_c$cluster<- y_kmeans

library(pheatmap)

heatmapping<- function(duomenys, c) {
  data_c1<- duomenys %>% filter(cluster == c)
  means_1<-matrix(colMeans(data_c1[,-ncol(data_c1)]), ncol = sqrt(784), byrow = TRUE)
  pheatmap(means_1, cluster_rows = FALSE, cluster_cols = FALSE,
           main = paste(as.character(c),"klasteris"))
}
# load libraries
library(ggplotify)
library(pheatmap)
library(patchwork)

p1<-as.ggplot(heatmapping(data_c, 1))
p2<-as.ggplot(heatmapping(data_c, 2))
p3<-as.ggplot(heatmapping(data_c, 3))
p4<-as.ggplot(heatmapping(data_c, 4))
p5<-as.ggplot(heatmapping(data_c, 5))
p6<-as.ggplot(heatmapping(data_c, 6))
p7<-as.ggplot(heatmapping(data_c, 7))
p8<-as.ggplot(heatmapping(data_c, 8))
p9<-as.ggplot(heatmapping(data_c, 9))
p10<-as.ggplot(heatmapping(data_c, 10))
p11<-as.ggplot(heatmapping(data_c, 11))
p1 + p2 + p3 + p4 + p5 +p6 + p7 + p8 + p9 + p10 + p11

######### dazniu lentele pagal kluster

testas <- data
testas$clust <- y_kmeans

# Funkcija gauti pasiskirstyma labels naujuose klasteriuose
lent <- function(duomenys, klasteris){
  temp1 <- duomenys %>% filter(clust == klasteris) %>% group_by(Label) %>%
    summarise(kiek = n())
  n_clust <- duomenys %>% filter(clust == klasteris) %>% nrow()
  temp1$kiek <- round(temp1$kiek/n_clust,2)
  temp1
}

temp <- lent(testas, 1)
temp <- merge(temp, lent(testas,2), by="Label", all = T)
temp <- merge(temp, lent(testas,3), by="Label", all = T)
temp <- merge(temp, lent(testas,4), by="Label", all = T)
temp <- merge(temp, lent(testas,5), by="Label", all = T)
temp <- merge(temp, lent(testas,6), by="Label", all = T)
temp <- merge(temp, lent(testas,7), by="Label", all = T)
temp <- merge(temp, lent(testas,8), by="Label", all = T)
temp <- merge(temp, lent(testas,9), by="Label", all = T)
temp <- merge(temp, lent(testas,10), by="Label", all = T)
temp <- merge(temp, lent(testas,11), by="Label", all = T)

temp[is.na(temp)] <- 0
names(temp) <- c("OG", "Klast 1", "Klast 2", "Klast 3", "Klast 4", "Klast 5",
                 "Klast 6", "Klast 7", "Klast 8", "Klast 9", "Klast 10", "Klast 11")
gal_klast_pas <- as.data.frame(t(temp))
gal_klast_pas <- gal_klast_pas[-1,]
names(gal_klast_pas) <- c("OG 0", "OG 1", "OG 2", "OG 3", "OG 4", "OG 5", "OG 6",
                          "OG 7", "OG 8", "OG 9")

write.csv(gal_klast_pas, file = "lent1.csv")

########## T-SNE ir KLASTERIZAVIMAS ##############

##### KLASTERIU SKAICIAI
#ELBOW
fviz_nbclust(data_1, kmeans, method = "wss", k.max = 15)

# Siluete
fviz_nbclust(data_1, kmeans, method = "silhouette", k.max = 15) +
  labs(subtitle = "Silhouette method")


##### K-means su naujais 2D dataframe
set.seed(67)
km.res2 <- kmeans(x = data_1, centers = 10)
y_kmeans2 = km.res2$cluster

# heatmapas
data_c2 <- data1
data_c2$cluster<- y_kmeans2

p1<- as.ggplot(heatmapping(data_c2, 1))
p2<- as.ggplot(heatmapping(data_c2, 2))
p3<- as.ggplot(heatmapping(data_c2, 3))
p4<- as.ggplot(heatmapping(data_c2, 4))
p5<- as.ggplot(heatmapping(data_c2, 5))
p6<- as.ggplot(heatmapping(data_c2, 6))
p7<- as.ggplot(heatmapping(data_c2, 7))
p8<- as.ggplot(heatmapping(data_c2, 8))
p9<- as.ggplot(heatmapping(data_c2, 9))
p10<- as.ggplot(heatmapping(data_c2, 10))
p1+ p2+p3+p4+p5+p6+p7+p8+p9+p10

##### Vizualizacija taskine diagrama
data_2 <- as.data.frame(data_1)

fviz_cluster(km.res2, data_2[,-3], stand = FALSE, geom = 'point')

######### dazniu lentele pagal kluster

testas2 <- data
testas2$clust <- y_kmeans2


temp2 <- lent(testas2, 1)
temp2 <- merge(temp2, lent(testas2, 2), by="Label", all = T)
temp2 <- merge(temp2, lent(testas2, 3), by="Label", all = T)
temp2 <- merge(temp2, lent(testas2, 4), by="Label", all = T)
temp2 <- merge(temp2, lent(testas2, 5), by="Label", all = T)
temp2 <- merge(temp2, lent(testas2, 6), by="Label", all = T)
temp2 <- merge(temp2, lent(testas2, 7), by="Label", all = T)
temp2 <- merge(temp2, lent(testas2, 8), by="Label", all = T)
temp2 <- merge(temp2, lent(testas2, 9), by="Label", all = T)
temp2 <- merge(temp2, lent(testas2, 10), by="Label", all = T)

temp2[is.na(temp2)] <- 0
names(temp2) <- c("OG", "Klast 1", "Klast 2", "Klast 3", "Klast 4", "Klast 5",
                 "Klast 6", "Klast 7", "Klast 8", "Klast 9", "Klast 10")
gal_klast_pas2 <- as.data.frame(t(temp2))
gal_klast_pas2 <- gal_klast_pas2[-1,]
names(gal_klast_pas2) <- c("OG 0", "OG 1", "OG 2", "OG 3", "OG 4", "OG 5", "OG 6",
                          "OG 7", "OG 8", "OG 9")
write.csv(x = gal_klast_pas2, file = "lent2.csv")





################################
# HIERARCHINIS METODAS
################################

############## Be t-sne ################

# Naudojant dendograma, identifikuojamas optimalus klasteriu skaicius
set.seed(67)
dendrogram = hclust(d = dist(data1, method = 'euclidean'), method = 'ward.D')
plot(dendrogram,
     main = paste('Dendograma'),
     xlab = 'Pikseliai',
     ylab = 'Euklidinis atstumas')

# Duomenys klasterizuojami hierarchiniu algoritmu
set.seed(67)
hc = hclust(d = dist(data1, method = 'euclidean'), method = 'ward.D')
y_hc = cutree(hc, 8)

# Nuspalvina dendograma
plot(hc)
rect.hclust(hc , k = 8, border = 2:6)


# heatmapai
data_c_hc <- data1
data_c_hc$cluster<- y_hc


p1<- as.ggplot(heatmapping(data_c_hc, 1))
p2<- as.ggplot(heatmapping(data_c_hc, 2))
p3<- as.ggplot(heatmapping(data_c_hc, 3))
p4<- as.ggplot(heatmapping(data_c_hc, 4))
p5<- as.ggplot(heatmapping(data_c_hc, 5))
p6<- as.ggplot(heatmapping(data_c_hc, 6))
p7<- as.ggplot(heatmapping(data_c_hc, 7))
p8<- as.ggplot(heatmapping(data_c_hc, 8))
#p9<- as.ggplot(heatmapping(data_c_hc, 9))
#p10<- as.ggplot(heatmapping(data_c_hc, 10))

p1+ p2+p3+p4+p5+p6+p7+p8#+p9+p10


### vizualizavimas taskine diagrama
#t-sne
fviz_cluster(list(data = a, cluster = y_hc), stand = F , geom = "point")


######### dazniu lentele pagal kluster

testas3 <- data
testas3$clust <- y_hc


temp3 <- lent(testas3, 1)
temp3 <- merge(temp3, lent(testas3, 2), by="Label", all = T)
temp3 <- merge(temp3, lent(testas3, 3), by="Label", all = T)
temp3 <- merge(temp3, lent(testas3, 4), by="Label", all = T)
temp3 <- merge(temp3, lent(testas3, 5), by="Label", all = T)
temp3 <- merge(temp3, lent(testas3, 6), by="Label", all = T)
temp3 <- merge(temp3, lent(testas3, 7), by="Label", all = T)
temp3 <- merge(temp3, lent(testas3, 8), by="Label", all = T)
#temp3 <- merge(temp3, lent(testas3, 9), by="Label", all = T)
#temp3 <- merge(temp3, lent(testas3, 10), by="Label", all = T)

temp3[is.na(temp3)] <- 0
names(temp3) <- c("OG", "Klast 1", "Klast 2", "Klast 3", "Klast 4", "Klast 5",
                  "Klast 6", "Klast 7", "Klast 8")#, "Klast 9", "Klast 10")
gal_klast_pas3 <- as.data.frame(t(temp3))
gal_klast_pas3 <- gal_klast_pas3[-1,]
names(gal_klast_pas3) <- c("OG 0", "OG 1", "OG 2", "OG 3", "OG 4", "OG 5", "OG 6",
                           "OG 7", "OG 8", "OG 9")
write.csv(gal_klast_pas3, "lent3.csv")

################ po t-SNE ##################
# Naudojant dendograma, identifikuojamas optimalus klasteriu skaicius
set.seed(67)
hc_tsne = hclust(d = dist(data_1, method = 'euclidean'), method = 'ward.D')
y_hc_tsne = cutree(hc_tsne, 8)

# Dendograma
# Nuspalvina dendograma
plot(hc_tsne)
rect.hclust(hc_tsne , k = 8, border = 2:6)

### APRASOMOJI
data_1_c_hc<- data1
data_1_c_hc$cluster<-y_hc_tsne

p1<- as.ggplot(heatmapping(data_1_c_hc, 1))
p2<- as.ggplot(heatmapping(data_1_c_hc, 2))
p3<- as.ggplot(heatmapping(data_1_c_hc, 3))
p4<- as.ggplot(heatmapping(data_1_c_hc, 4))
p5<- as.ggplot(heatmapping(data_1_c_hc, 5))
p6<- as.ggplot(heatmapping(data_1_c_hc, 6))
p7<- as.ggplot(heatmapping(data_1_c_hc, 7))
p8<- as.ggplot(heatmapping(data_1_c_hc, 8))
#p9<- as.ggplot(heatmapping(data_1_c_hc, 9))
#p10<- as.ggplot(heatmapping(data_1_c_hc, 10))

p1+ p2+p3+p4+p5+p6+p7+p8#+p9+p10

a_tsne<-matrix(unlist(data_1), nrow = 2000, ncol = 3)
fviz_cluster(list(data = a_tsne, cluster = y_hc_tsne), stand = FALSE, geom = "point")


######### dazniu lentele pagal kluster
testas4 <- data
testas4$clust <- y_hc_tsne

temp4 <- lent(testas4, 1)
temp4 <- merge(temp4, lent(testas4, 2), by="Label", all = T)
temp4 <- merge(temp4, lent(testas4, 3), by="Label", all = T)
temp4 <- merge(temp4, lent(testas4, 4), by="Label", all = T)
temp4 <- merge(temp4, lent(testas4, 5), by="Label", all = T)
temp4 <- merge(temp4, lent(testas4, 6), by="Label", all = T)
temp4 <- merge(temp4, lent(testas4, 7), by="Label", all = T)
temp4 <- merge(temp4, lent(testas4, 8), by="Label", all = T)
#temp4 <- merge(temp4, lent(testas4, 9), by="Label", all = T)
#temp4 <- merge(temp4, lent(testas4, 10), by="Label", all = T)

temp4[is.na(temp4)] <- 0
names(temp4) <- c("OG", "Klast 1", "Klast 2", "Klast 3", "Klast 4", "Klast 5",
                  "Klast 6", "Klast 7", "Klast 8")#, "Klast 9", "Klast 10")
gal_klast_pas4 <- as.data.frame(t(temp4))
gal_klast_pas4 <- gal_klast_pas4[-1,]
names(gal_klast_pas4) <- c("OG 0", "OG 1", "OG 2", "OG 3", "OG 4", "OG 5", "OG 6",
                           "OG 7", "OG 8", "OG 9")
write.csv(gal_klast_pas4, "lent4.csv")

###########################
# HIERARCHINIS MAZAI KLASTERIU
###########################
################ po t-SNE ##################
# Naudojant dendograma, identifikuojamas optimalus klasteriu skaicius
set.seed(67)
hc_tsne = hclust(d = dist(data_1, method = 'euclidean'), method = 'ward.D')
y_hc_tsne = cutree(hc_tsne, 3)

# Dendograma
# Nuspalvina dendograma
plot(hc_tsne)
rect.hclust(hc_tsne , k = 3, border = 2:6)

# Taskine diagrama
a_tsne<-matrix(unlist(data_1), nrow = 2000, ncol = 3)
fviz_cluster(list(data = a_tsne, cluster = y_hc_tsne), stand = FALSE, geom = "point")

### APRASOMOJI
data_1_c_hc<- data1
data_1_c_hc$cluster<-y_hc_tsne

p1<- as.ggplot(heatmapping(data_1_c_hc, 1))
p2<- as.ggplot(heatmapping(data_1_c_hc, 2))
p3<- as.ggplot(heatmapping(data_1_c_hc, 3))

p1+p2+p3



######### dazniu lentele pagal kluster
testas4 <- data
testas4$clust <- y_hc_tsne

temp4 <- lent(testas4, 1)
temp4 <- merge(temp4, lent(testas4, 2), by="Label", all = T)
temp4 <- merge(temp4, lent(testas4, 3), by="Label", all = T)


temp4[is.na(temp4)] <- 0
names(temp4) <- c("OG", "Klast 1", "Klast 2", "Klast 3")
gal_klast_pas4 <- as.data.frame(t(temp4))
gal_klast_pas4 <- gal_klast_pas4[-1,]
names(gal_klast_pas4) <- c("OG 0", "OG 1", "OG 2", "OG 3", "OG 4", "OG 5", "OG 6",
                           "OG 7", "OG 8", "OG 9")

write.csv(gal_klast_pas4, "lent5.csv")

###########################
# GALUTINE LENTELE
###########################

galut <- data[,785]
galut$kmeans <- y_kmeans
galut$kmeans_tsne <- y_kmeans2
galut$hc <- y_hc
galut$hc_tsne <- y_hc_tsne