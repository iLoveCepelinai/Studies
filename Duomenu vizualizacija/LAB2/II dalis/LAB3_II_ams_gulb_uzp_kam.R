# TVARKOME DUOMENIS

library("readxl")
duom<-read_excel("namai.xlsx")

names(duom)<-c("nr", "pirkimas", "amzius", "atstum_metro", "pard_sk", "platuma", "ilguma", "kaina")

duomenys<-subset(duom, select=c(amzius, atstum_metro, pard_sk, platuma, ilguma, kaina))

duomenys$kainos_iv <- as.factor(ifelse(duomenys$kaina < 25, "Pigu", ifelse(duomenys$kaina < 60, "Vidutiniðka",
                                                                           "Brangu")))
duomenys$kainos_iv <- factor(duomenys$kainos_iv, levels = c("Pigu", "Vidutiniðka", "Brangu"), order = TRUE)

library(ggplot2)
library(viridis)
library(ggimage)

img="gg.jpg"

temp<-subset(duomenys, kaina<80)

# PIRMA PANELE

# pirmas grafikas p1
p1<-ggplot(data = temp, aes(ilguma, platuma, color=kaina)) + geom_point(size=2.3) + scale_color_viridis("Kaina") + 
  ggtitle("Namø geografinis pasiskirstymas ir kainos") + labs(x="Ilguma", y="Platuma") + 
    theme(legend.position = c(0.93,0.7), legend.background = element_rect(fill = "transparent"));p1


# pirmas su nuotrauka p12
bandymas<-ggplot(data = temp, aes(ilguma, platuma, color=kaina)) + geom_point(size=2.3) + scale_color_viridis() + 
  theme(legend.position = "none", axis.title.x=element_blank(), axis.text.x=element_blank(),
        axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank(), 
        axis.ticks.x=element_blank(), panel.border = element_rect(color = "black", fill = NA, size = 1.5))
p12<-ggbackground(bandymas, img);p12

# antras grafikas p2
library(lattice)
p2<-xyplot(duomenys$atstum_metro~duomenys$pard_sk|duomenys$kainos_iv, data=duomenys, group = duomenys$kainos_iv,
           main="Pasiskirstymo grafikas pagal namø kainas", pch = 19, cex = 1.5,
           par.settings=list(par.main.text=list(font = 1, y=-0.5)),
           col = c("#003380", "dark green", "yellow"), alpha = 0.2,
           ylab="Atstumas iki metro", xlab="Parduotuviø sk. ëjimo distancijoje", layout = c(1,3));p2

# trecias grafikas p3
library("GGally")
p3<-ggparcoord(duomenys, columns = 1:3, groupColumn = 7, scale="uniminmax") + ggtitle("Lygiagreèiosios koordinatës") + 
  theme(plot.title = element_text(hjust = 0.5), legend.position = c(0.92,0.5) ,axis.title.y=element_blank(), 
        legend.key.size = unit(1, "cm"),
        axis.text.y=element_blank(), panel.background = element_blank(), axis.ticks.y=element_blank())+ labs(x="")+
  scale_color_manual("Kainø kategorijos", values=c("#003380", "dark green", "yellow")) + 
  scale_x_discrete(labels=c("Amþius", "Atstumas iki metro", "Parduotuviø skaièius")) + 
  guides(color = guide_legend(override.aes = list(size = 2)));p3


# PIESIAMA PIRMA PANELE
# Paketas patchwork

library(patchwork)

c1<-p1 + inset_element(p12, left = 0.015, bottom = 0.45, right = 0.445, top = 0.99);c1

# Paketas cowplot

library(cowplot)

virsus<-plot_grid(c1, p2)
panele1temp<-plot_grid(virsus, p3, ncol = 1);panele1temp

pavadinimas <- ggdraw() + draw_label("Skirtingø kainø tendencijos",fontface = 'bold',x = 0.075,hjust = 0)
antraste<- ggdraw() + draw_label("Panelë 1", x = 0.075,hjust = 0,vjust=0.3, size=12)
paraste<-ggdraw() + draw_label("Padaryta su patchwork ir cowplot", size = 10, x = 0.9, hjust = 0.65, vjust=-0.08)
panele1<-plot_grid(pavadinimas,antraste,panele1temp,paraste,ncol = 1, rel_heights = c(0.04,0.015,1,0.007))
ggdraw(panele1)


# ANTRA PANELE

#trecias grafikas
p21<-ggplot(data=duomenys, aes(kaina)) + geom_histogram(binwidth=10, color = "black", fill = "white") + 
  labs(x="Kaina",y="Daþnis")+ ggtitle("Namø kainø histograma") + 
  geom_vline(aes(xintercept = mean(kaina)), linetype="dashed",col = "blue")+
  geom_text(aes(x=mean(kaina), label=paste("Vidurkis",round(mean(kaina),2)), y=50), colour="blue",angle=90,vjust = 1.2)
p21

#ketvirtas grafikas
p22<-ggplot(data=duomenys, aes(kaina, amzius)) + stat_density2d(aes(fill=..level..), geom="polygon") +
  scale_fill_gradient(low="blue", high="yellow") + labs(fill="Tankis")+
  ggtitle("Namø pasiskirstymas pagal kainà ir amþiø") + labs(x="Kaina", y="Namo amþius") + 
  theme(legend.position = c(0.9,0.8), legend.background = element_rect(fill = "transparent"));p22


# PIESIAMA ANTRA PANELE

panele2<-p21+p22+plot_annotation(
  title="Kainos pasiskirstymai",
  subtitle="Panelë 2",
  caption="Padaryta su patchwork");panele2

paneltest<-panele2 & theme_bw()
paneltest + plot_annotation(theme = theme(plot.title = element_text(size = 18)), tag_levels = "1")
