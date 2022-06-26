#Skaitome ir tvarkome duomenis
duom<-read.csv("masinos.csv")
duomenys<-subset(duom, select = c(1, 15, 17, 21, 28, 31, 52, 54, 55, 60, 65, 70, 71))
names(duomenys)<-c("Marke", "Kategorija", "Kebulas", "Mase", "Galia", "Degalai", "Spalva", "Greitis", "Vietos", "Reg_data",
              "Salis", "Savivaldybe", "Apskritis")
duomenys1<-subset(duomenys, Kategorija == "M1")
duomenys1<-subset(duomenys, Marke != "Nuasmeninta")
duomenys1[!is.na(duomenys$Apskritis),]
duomenys1[!is.na(duomenys$Savivaldybe),]

#Irasome pakeistus duomenis
write.csv(duomenys1,"masinos_red.csv")


# Kodas su sutvarkytu failu
duom<-read.csv("masinos_red.csv")

library(sf)
library(ggplot2)
library(GADMTools)
library(sp)
library(viridis)

duom<-subset(duom, Marke != "Nuasmeninta")

rajonai <- duom$Savivaldybe
rajonai<-unique(rajonai)
rajonai<-sort(rajonai)
rajonai<-rajonai[2:67]
rajonai<-rajonai[!is.na(rajonai)]

#??????
rajonai<-rajonai[rajonai !="JUNGTINE KARALYSTE.  "]
rajonai<-rajonai[rajonai !="KANADA.  "]
rajonai<-rajonai[rajonai !="LATVIJA.  "]
rajonai<-rajonai[rajonai !="UKRAINA.  "]
rajonai<-rajonai[rajonai !="ANGILIJA.  "]
rajonai<-rajonai[rajonai !="BALTARUSIJA.  "]


rajonai<-rajonai[rajonai !="ALYTAUS M. SAV."]
rajonai<-rajonai[rajonai !="KAUNO M. SAV."]
rajonai<-rajonai[rajonai !="KLAIPEDOS M. SAV."]
rajonai<-rajonai[rajonai !="PANEVEZIO M. SAV."]
rajonai<-rajonai[rajonai !="SIAULIU M. SAV."]
rajonai<-rajonai[rajonai !="VILNIAUS M. SAV."]
rajonai<-rajonai[rajonai !="ELEKTRENU SAV."]
rajonai<-rajonai[rajonai !="KALVARIJOS SAV."]
rajonai<-rajonai[rajonai !="KAZLU RUDOS SAV."]
rajonai<-rajonai[rajonai !="PAGEGIU SAV."]
rajonai<-rajonai[rajonai !="RIETAVO SAV."]
rajonai<-rajonai[rajonai !="VISAGINO SAV."]


red_sav<-replace(duom$Savivaldybe,duom$Savivaldybe=="ALYTAUS M. SAV.","ALYTAUS R. SAV.")
red_sav<-replace(red_sav,red_sav=="KAUNO M. SAV.","KAUNO R. SAV.")
red_sav<-replace(red_sav,red_sav=="KLAIPEDOS M. SAV.","KLAIPEDOS R. SAV.")
red_sav<-replace(red_sav,red_sav=="PANEVEZIO M. SAV.","PANEVEZIO R. SAV.")
red_sav<-replace(red_sav,red_sav=="SIAULIU M. SAV.","SIAULIU R. SAV.")
red_sav<-replace(red_sav,red_sav=="VILNIAUS M. SAV.","VILNIAUS R. SAV.")

duom<-cbind(duom, red_sav)

lietuva<-st_read("gadm36_LTU.gpkg")

duom<-subset(duom, duom$red_sav %in% rajonai)

graz_pav<-lietuva$NAME_2
graz_pav<-sort(graz_pav)

duom<-duom[order(duom$red_sav),]
duom$sav_graz<-factor(duom$red_sav,labels=graz_pav)

duom$Metai<-strtoi(substring(duom$Reg_data,1,4))

#####################################################################################3
#Duomenu sumavimas

get_mode <- function(x, vieta){
  return(names(sort(table(x), decreasing = T, na.last = T)[vieta]))
}

dat <- data.frame(stringsAsFactors = FALSE)

i<-0
for(raj in graz_pav){
  i<-i+1
  temp<-subset(duom, duom$sav_graz==raj)
  dat[i,1]<-graz_pav[i]
  dat[i,2]<-get_mode(temp$Marke, 1)
  dat[i,3]<-nrow(temp)
  dat[i,4]<-get_mode(temp$Spalva, 4)
  dat[i,5]<-mean(temp$Galia,na.rm=TRUE,trim = 0.03)
  dat[i,6]<-get_mode(temp$Apskritis, 1)
  dat[i,7]<-round(mean(temp$Metai, na.rm=TRUE))
}

names(dat)<-c("Savivaldybe", "Marke", "Kiekis", "Spalva", "Galia", "Apskritis", "Metai")


lt<-lietuva[order(lietuva$NAME_2),]
lt[,"Apskritis"]<-dat$Apskritis
lt[,"Galia"]<-dat$Galia
lt[,"Kiekis"]<-dat$Kiekis
lt[,"Metai"]<-dat$Metai

##############################################################
#Grafiku piesimas

#1
ggplot(data=lt)+geom_sf(aes(fill=dat$Marke))+ ggtitle("Populiariausios markës savivaldybëse")+labs(fill="Markë")+
  theme(axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks.x = element_blank(), 
        axis.ticks.y = element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank())S

#2
library(scales)
og2<-ggplot(data=lt)+geom_sf(aes(fill=dat$Kiekis)) +
    labs(fill="Maðinø sk.")+scale_fill_gradient(low="white", high="red", labels=comma)+
  theme(axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks.x = element_blank(), 
        axis.ticks.y = element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank());og2

temp2<-dat
temp2[,7] <- log(temp2[,3])
names(temp2)<-c("Savivaldybe", "Marke", "Kiekis", "Spalva", "Galia", "Apskritis", "Log")

logor2<-ggplot(data=lt)+geom_sf(aes(fill=temp2$Log))+
  labs(fill="Maðinø sk.")+scale_fill_gradient(low="white", high="red", labels=comma)+
  theme(axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks.x = element_blank(), 
        axis.ticks.y = element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank());logor2

library(ggpubr)

kombo<-ggarrange(og2,logor2,nrow = 2);kombo
annotate_figure(kombo,top=text_grob("Automobiliø skaièius pagal savivaldybes"))

#3
library(tmap)

tm_shape(lt) + tm_polygons("Galia")+
  tm_borders() + tm_layout(main.title = "Automobiliø galios savivaldybëse", main.title.size = 1.2)+
  tm_facets(by = "NAME_1")+tm_layout(legend.position=c(-1.4, "bottom"), panel.label.size = 1.5,
            legend.format = list(text.separator="iki"))+
  tm_text("NAME_2", size=1.2)

#4
tmap_mode("plot")
tm_shape(lt) + tm_polygons("Metai", palette = "YlGnBu") + tm_borders() + 
  tm_layout(main.title = "Automobiliø amþius savivaldybëse", 
            main.title.size = 1.2, legend.format=list(fun=function(x) formatC(x, digits=0, format="d")))
