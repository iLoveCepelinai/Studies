# Duomenu tvarkymas

library("readxl")
duom<-read_excel("namai.xlsx")

head(duom)
names(duom)<-c("nr", "pirkimas", "amzius", "atstum_metro", "pard_sk", "platuma", "ilguma", "kaina")

duomenys<-subset(duom, select=c(amzius, atstum_metro, pard_sk, platuma, ilguma, kaina))
head(duomenys)

# Grafiku sudarimas
library(ggplot2)

install.packages("viridis")
library(viridis)

library(ggimage)
img="gg.jpg"

temp<-subset(duomenys, kaina<80)

# pirmas grafikas
ggplot(data = temp, aes(ilguma, platuma, color=kaina)) + geom_point(size=2.3) + scale_color_viridis("Kaina") + 
  ggtitle("Namø geografinis pasiskirstymas ir kainos") + labs(x="Ilguma", y="Platuma")


# pirmas su nuotrauka
bandymas<-ggplot(data = temp, aes(ilguma, platuma, color=kaina)) + geom_point(size=2.3) + scale_color_viridis() + 
  theme(legend.position = "none", axis.title.x=element_blank(), axis.text.x=element_blank(),
        axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank(), 
          axis.ticks.x=element_blank())
ggbackground(bandymas, img)


#antras grafikas
duomenys$kainos_iv <- as.factor(ifelse(duomenys$kaina < 25, "Pigu", ifelse(duomenys$kaina < 60, "Vidutiniðka",
                                                                           "Brangu")))
summary(duomenys$platuma)
head(duomenys)

duomenys$kainos_iv <- factor(duomenys$kainos_iv, levels = c("Pigu", "Vidutiniðka", "Brangu"), order = TRUE)

library(lattice)
xyplot(duomenys$atstum_metro~duomenys$pard_sk|duomenys$kainos_iv, data=duomenys,
       main="Pasiskirstymo grafikas pagal namø kainas",
       ylab="Atstumas iki metro", xlab="Parduotuviø sk. ëjimo distancijoje", layout = c(1,3))

#trecias grafikas
ggplot(data=duomenys, aes(kaina)) + geom_histogram(binwidth=10, color = "black", fill = "white") + 
  labs(x="Kaina",y="Daþnis")+ ggtitle("Namø kainø histograma") + 
  geom_vline(aes(xintercept = mean(kaina)), linetype="dashed",col = "blue")+
  geom_text(aes(x=mean(kaina), label=paste("Vidurkis",round(mean(kaina),2)), y=50), colour="blue", angle=90, vjust = 1.2)


#ketvirtas grafikas
ggplot(data=duomenys, aes(kaina, amzius)) + stat_density2d(aes(fill=..level..), geom="polygon") +
        scale_fill_gradient(low="blue", high="yellow") + labs(fill="Tankis")+
      ggtitle("Namø pasiskirstymas pagal kainà ir amþiø") + labs(x="Kaina", y="Namo amþius")


#############################################################################################

# 2 uzduotele

install.packages("GGally")
library("GGally")
ggparcoord(duomenys, columns = 1:3, groupColumn = 7, scale="uniminmax") + ggtitle("Lygiagreèiosios koordinatës") + 
    theme(axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank())+ labs(x="")+
      scale_color_manual("Kainø kategorijos", values=c("#003380", "dark green", "yellow")) + 
        scale_x_discrete(labels=c("Amþius", "Atstumas iki metro", "Parduotuviø skaièius"))


# 3 uzduotele

library(aplpack)

faces(duomenys[1:25,1:6])

# 4 uzduotele

library(corrplot)

duom4 <- subset(duomenys, select=c(amzius, atstum_metro, pard_sk, kaina))

M <-cor(duom4)

colnames(M) <- c("Amþius", "Atstumas\n iki metro", "Parduotuviø\n skaièius", "Kaina")
rownames(M) <- c("Amþius", "Atstumas\n iki metro", "Parduotuviø\n skaièius", "Kaina")

corrplot(M, method = "circle")

corrplot(M, method = "shade")
