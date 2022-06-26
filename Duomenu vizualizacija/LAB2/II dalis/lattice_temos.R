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

temp<-subset(duomenys, kaina<80)

# PIRMA PANELE

# pirmas grafikas p1
p1<-ggplot(data = temp, aes(ilguma, platuma, color=kaina)) + geom_point(size=2.3) + scale_color_viridis("Kaina") + 
  ggtitle("Namø geografinis pasiskirstymas ir kainos") + labs(x="Ilguma", y="Platuma") + 
    theme(legend.position = c(0.93,0.7), legend.background = element_rect(fill = "transparent"));p1


# antras grafikas p2
library(lattice)

mytheme = trellis.par.get()
mytheme$panel.background$col = "grey80"
trellis.par.set(mytheme)

# Paleidus cia, uzsideda pilkas fonas ir viskas atrodo veikia. Grafikas irasomas i p2, kuris veliau panaudojamas
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

# Paketas cowplot

library(cowplot)

# Paleidus 59 eilute jau nebeveikia pilkas fonas (bent man). Jei nerodo grafiko spauskite zoom arba export ir tada
# pasireguliuokite masteli

virsus<-plot_grid(p1, p2)
panele1temp<-plot_grid(virsus, p3, ncol = 1);panele1temp

# kodas zemiau jau nebutinas bet idedu viska del viso ko
pavadinimas <- ggdraw() + draw_label("Skirtingø kainø tendencijos",fontface = 'bold',x = 0.075,hjust = 0)
antraste<- ggdraw() + draw_label("Panelë 1", x = 0.075,hjust = 0,vjust=0.3, size=12)
paraste<-ggdraw() + draw_label("Padaryta su patchwork ir cowplot", size = 10, x = 0.9, hjust = 0.65, vjust=-0.08)
panele1<-plot_grid(pavadinimas,antraste,panele1temp,paraste,ncol = 1, rel_heights = c(0.04,0.015,1,0.007))
ggdraw(panele1)
