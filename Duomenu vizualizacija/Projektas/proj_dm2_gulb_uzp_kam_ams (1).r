library(readr)
library(ggplot2)
library(patchwork)
library(scales)
library(dplyr)
library(ggvis)
Sys.setlocale("LC_ALL", "Lithuanian")
#nuskaitome ir persitvarkome duomenis

#a)
######################################################
#papildomai su b) leisti sia dali tik tada, jei neturima apdarotu duomenu (tai yra turimas failas is nurodytos svetaines)
NZraw<-read_csv("C:/Users/Acer/Desktop/duomenys/Fleet-30Apr2021.csv")
NZ<-subset(NZraw, NZraw$VEHICLE_USAGE=="PRIVATE PASSENGER"&
           (NZraw$VEHICLE_TYPE=="PASSENGER CAR/VAN"|NZraw$VEHICLE_TYPE=="GOODS VAN/TRUCK/UTILITY")&
           NZraw$NUMBER_OF_SEATS<10&NZraw$VEHICLE_YEAR>1999)
NZ<-subset(NZ, select = c(3,4,5,11,16, 17, 18, 21, 33))
write.csv(NZ, "NZ2.csv")
######################################################

#b)
######################################################
#Jei jau turimas apdorotas pridetas failas, leisti sia kodo dali
NZ<-read_csv("NZ2.csv")
NZ<-subset(NZ, (NZ$GROSS_VEHICLE_MASS<8000 & NZ$GROSS_VEHICLE_MASS>1000) | is.na(NZ$GROSS_VEHICLE_MASS))
######################################################



#a)
######################################################
#papildomai su b) leisti sia dali tik tada, jei neturima apdarotu duomenu (tai yra turimi tik 4 csv failai is 
#nurodytos svetaines)
duom_A1<-read_csv("C:/Users/Acer/Desktop/duomenys/vehicleregistrationslightvehiclespart1.csv")
duom_A2<-read_csv("C:/Users/Acer/Desktop/duomenys/vehicleregistrationslightvehiclespart2.csv")
duom_A3<-read_csv("C:/Users/Acer/Desktop/duomenys/vehicleregistrationslightvehiclespart3.csv")
duom_A4<-read_csv("C:/Users/Acer/Desktop/duomenys/vehicleregistrationslightvehiclespart4.csv")
AUraw<-rbind(duom_A1, duom_A2, duom_A3, duom_A4)
AU<-subset(AUraw,AUraw$`Number of Seats`<10 & (AUraw$`Registration Category`== "LIGHT COML OR VAN"|
          AUraw$`Registration Category`== "PASSENGER CAR")& AUraw$`Purpose of Use`=="PRIVATE"&
             AUraw$`Year of Manufacture`>1999, select=c(2,4,5,6,7,8,9,10,11,12,16))
write.csv(AU, "AU.csv")
######################################################


#b)
######################################################
#Jei jau turimas apdorotas pridetas failas, leisti tik sia kodo dali
AU<-read_csv("AU2.csv")
AU<-subset(AU, (AU$`GVM Weight`>1000 & AU$`GVM Weight`<8000) | is.na(AU$`GVM Weight`))
######################################################



##########################################################################################
#pakeiciame N.Zelandijos spalvas
NZ1<-NZ
NZ1<-subset(NZ1, !is.na(NZ1$BASIC_COLOUR))
NZ1$BASIC_COLOUR<-replace(NZ1$BASIC_COLOUR,NZ1$BASIC_COLOUR=="GOLD","ORANGE")
NZ1$BASIC_COLOUR<-replace(NZ1$BASIC_COLOUR,NZ1$BASIC_COLOUR=="SILVER","GREY")

#N.Zelandijos spalvu stulpeline diagrama

spalvosNZ1<-ggplot(data=NZ1, aes(BASIC_COLOUR, fill=BASIC_COLOUR)) +
  geom_bar(aes(y = (..count..)/sum(..count..)), color="black")+
  scale_y_continuous(labels=scales::percent) + 
  theme(legend.position = "none",axis.text.x=element_blank(), axis.ticks.x=element_blank())+
  labs(x="Spalva",y="Dažnis")+ ggtitle("N. Zelandijos automobiliai")+ 
  scale_fill_manual(values=c(GREY="GREY", BROWN ="BROWN", GREEN="GREEN",
                             RED="RED", WHITE="WHITE", BLUE="BLUE",
                             BLACK="BLACK", GOLD="GOLD", YELLOW="YELLOW",
                             PURPLE="PURPLE", ORANGE="ORANGE", CREAM="#fffdd0",
                             PINK="PINK"));spalvosNZ1
#pakeiciame Australijos spalvas
AU1<-AU
AU1$Colour<-replace(AU$Colour,AU$Colour=="MAROON"|AU$Colour=="TAN"|AU$Colour=="BRONZE"|
                      AU$Colour=="FAWN","BROWN")
AU1$Colour<-replace(AU1$Colour,AU1$Colour=="BEIGE"|AU1$Colour=="KHAKI","CREAM")
AU1$Colour<-replace(AU1$Colour,AU1$Colour=="MAUVE","PURPLE")
AU1$Colour<-replace(AU1$Colour,AU1$Colour=="TURQUOISE","BLUE")
AU1$Colour<-replace(AU1$Colour,AU1$Colour=="GOLD","ORANGE")
AU1$Colour<-replace(AU1$Colour,AU1$Colour=="SILVER","GREY")

#Australijos spalvu stulpeline diagrama

spalvosAU1<-ggplot(data=AU1, aes(Colour, fill=Colour)) + 
  geom_bar(aes(y = (..count..)/sum(..count..)), color="black")+
  scale_y_continuous( position = "right",labels=scales::percent) +  
  ggtitle("Australijos automobiliai")+ labs(x="Spalva")+
  theme(legend.position = "none",axis.text.x=element_blank(),axis.title.y=element_blank(), 
        axis.ticks.x=element_blank())+
  scale_fill_manual(values=c(YELLOW="YELLOW", WHITE="WHITE", CREAM="#fffdd0",
                             GREEN="GREEN", RED="RED", BLACK="BLACK", 
                             GREY="GREY",BROWN="BROWN", BLUE="BLUE", 
                             ORANGE="ORANGE", PURPLE="PURPLE", 
                             PINK="PINK"));spalvosAU1
#spalvu grafiku panele
spalvosNZ1+spalvosAU1+
  plot_annotation(title="Automobilių spalvų stulpelinės diagramos")&
  scale_y_continuous(labels=scales::percent, limits = c(0, 0.355))

#####################################################################################################
# pakeiciame Australijos kura
AU1$`Fuel Type`<-replace(AU1$`Fuel Type`, AU1$`Fuel Type`=="ELECTRIC"|AU1$`Fuel Type`=="STEAM"|
                           AU1$`Fuel Type`=="GAS"|AU1$`Fuel Type`=="PETROL AND GAS"|
                           AU1$`Fuel Type`=="DIESEL AND GAS"| AU1$`Fuel Type`=="DIESEL AND ELECTRIC"|
                           AU1$`Fuel Type`=="PETROL AND ELECTRIC","OTHER")

degAU<-as.data.frame(table(AU1$`Fuel Type`))
degsum<-sum(degAU$Freq)
mycols <- c("#0073C2FF", "#EFC000FF", "#CD534CFF")
degAU <- degAU %>%  arrange(desc(Var1)) %>%  mutate(lab.ypos = cumsum(Freq) - 0.5*Freq)
#Australijos kuro diagrama
DAU<-ggplot(degAU, aes(x = 2, y = Freq, fill = Var1)) +
  geom_bar(width = 1, stat = "identity", color = "black") + 
  coord_polar("y", start = 0) +
  geom_text(aes(y = lab.ypos, label = paste0(round(Freq/degsum, 4)*100, "%")),
            color = "black", size=7) +
  scale_fill_manual(values = mycols, labels=c("Dyzelis", "Kita", "Benzinas")) + 
  theme_void() +  ggtitle("Australija") +  labs(fill = "Kuras") + xlim(0.7, 2.5) +
  theme(plot.title = element_text(hjust = 0.5, vjust= -40, size=18), 
        legend.key.size = unit(1.5, "cm"), legend.key.width = unit(0.5,"cm"), 
        legend.title =element_text(size = 15), legend.text = element_text(size = 15));DAU

#pakeiciame N.Zelandijos kura
NZ1$MOTIVE_POWER<-replace(NZ1$MOTIVE_POWER, NZ1$MOTIVE_POWER=="CNG"|
                            NZ1$MOTIVE_POWER=="PETROL HYBRID"|
                            NZ1$MOTIVE_POWER=="ELECTRIC"|NZ1$MOTIVE_POWER=="PLUGIN PETROL HYBRID"|
                            NZ1$MOTIVE_POWER=="LPG"|NZ1$MOTIVE_POWER=="PETROL ELECTRIC HYBRID"|
                            NZ1$MOTIVE_POWER=="ELECTRIC [PETROL EXTENDED]"|
                            NZ1$MOTIVE_POWER=="ELECTRIC [DIESEL EXTENDED]"|
                            NZ1$MOTIVE_POWER=="DIESEL HYBRID"|NZ1$MOTIVE_POWER=="PLUGIN DIESEL HYBRID"|
                            NZ1$MOTIVE_POWER=="ELECTRIC FUEL CELL HYDROGEN"|
                            NZ1$MOTIVE_POWER=="DIESEL ELECTRIC HYBRID"|NZ1$MOTIVE_POWER=="CNG","OTHER")

degNZ<-as.data.frame(table(NZ1$MOTIVE_POWER))
degsumN<-sum(degNZ$Freq)
degNZ <- degNZ %>%  arrange(desc(Var1)) %>%  mutate(lab.ypos = cumsum(Freq) - 0.5*Freq)

#N.Zelandijos kuro diagrama
DNZ<-ggplot(degNZ, aes(x = 2, y = Freq, fill = Var1)) +
  geom_bar(width = 1, stat = "identity", color = "black") + coord_polar("y", start = 0)+
  geom_text(aes(y = lab.ypos, label = paste0(round(Freq/degsumN, 4)*100, "%")),color = "black", size=7) +
  scale_fill_manual(values = mycols, labels=c("Dyzelis", "Kita", "Benzinas")) +  theme_void() + 
  ggtitle("N.Zelandija") +  labs(fill = "Kuras") + xlim(0.7,2.5) +
  theme(plot.title = element_text(hjust = 0.5, vjust= -40, size=18),
        legend.key.size = unit(1.5, "cm"), legend.key.width = unit(0.5,"cm"), 
        legend.title =element_text(size = 15), legend.text = element_text(size = 15));
DNZ

# kuro grafiku panele
DNZ+DAU+
  plot_annotation(title="Automobilių kuro diagramos",
                  theme = theme(plot.title=element_text(size = 25, hjust=0.5))) +
  plot_layout(guides = "collect")


##################################################################################################
#atrenkam N.Zelandijos mase

NZ2<-subset(NZ1, `GROSS_VEHICLE_MASS`>1000 & `GROSS_VEHICLE_MASS`<2500, 
            select=c(5, 10))
#Atrenkame paprastaja atsitiktine imti, nes su visais duomenimis uzlusta kompiuteris
NZ2<-sample_frac(NZ2, size = 0.4)
# N.Zelandijos heatmap'as pagal mase ir metus
MNZ<-ggplot(data=NZ2, aes(`VEHICLE_YEAR`, `GROSS_VEHICLE_MASS`)) +
  stat_density2d(aes(fill=..level..), geom="polygon") +
  scale_fill_gradient(low="blue", high="yellow") + theme(legend.position = "none") + 
  ggtitle("N.Zelandija") +
  labs(x="Metai", y="Masė")
MNZ

# atrenkam Australijos mase
AU2<-subset(AU1, AU1$`GVM Weight`>1000 & AU1$`GVM Weight`<2500)
# Australijos heatmap'as pagal mase ir metus 
MAU<-ggplot(data=AU2, aes(`Year of Manufacture`,`GVM Weight`)) + 
  stat_density2d(aes(fill=..level..), geom="polygon") +
  scale_fill_gradient(low="blue", high="yellow") + 
  theme(legend.position = "none", axis.title.y=element_blank()) + 
  ggtitle("Australija") + labs(x="Metai")
MAU
# mases paneles
MNZ+MAU+plot_annotation(title="Automobilių bendrosios masės pasiskirstymas pagal metus",
                        theme = theme(plot.title=element_text(size = 25, hjust=0.5)))&
  scale_y_continuous(limits = c(1000, 2500))&scale_x_continuous(limits = c(1995,2020))


###################################################
# atrenkame automobilių kiekį pagal metus ir jo modelį

#Australijos grafikas

laik<-data.frame(sort(table(AU$Make), decreasing =T))
popAU<-as.vector(laik[1:5,1])    # atrenkam populiariausius automobilius
popAU
AUmet<-data.frame("1", 2, 3)    # pasidarome lentele i kuria rasysime automobiliu kieki marke ir metus
names(AUmet)<-c("Marke", "Metai", "Kiekis")

for(i in 2000:2019){
  metai<-subset(AU, `Year of Manufacture`==i)
  lentele<-data.frame(table(metai$Make))
  for(j in popAU){
    nr<-which(grepl(j, lentele$Var1))
    AUmet[nrow(AUmet) + 1,] = c(j,i, as.numeric(lentele[nr[1],2]))
  }                   #sudedam reikalingus duomenis į vieną lentelę
}
AUmet$Kiekis<-as.numeric(AUmet$Kiekis)
AUmet[is.na(AUmet)]<-0
AUmet<-AUmet[-1,]
AUM<-ggplot(AUmet, aes(x=Metai, y=Kiekis, group=Marke))+
  geom_line(aes(color=Marke), size=2) + scale_x_discrete(breaks=seq(2000,2020,2))+ 
  ggtitle("Australija")+ labs(color = "Markė")+
  theme(axis.title.y=element_blank())
AUM
#naujosios zelandijos grafikas

laik<-data.frame(sort(table(NZ$MAKE), decreasing =T))
popNZ<-as.vector(laik[1:5,1])    # atrenkam populiariausius automobilius
popNZ
NZmet<-data.frame("1", 2, 3)    # pasidarome lentele i kuria rasysime automobiliu kieki marke ir metus
names(NZmet)<-c("Marke", "Metai", "Kiekis")

for(i in 2000:2019){
  metai<-subset(NZ, VEHICLE_YEAR==i)
  lentele<-data.frame(table(metai$MAKE))
  for(j in popNZ){
    nr<-which(grepl(j, lentele$Var1))
    NZmet[nrow(NZmet) + 1,] = c(j,i, as.numeric(lentele[nr[1],2]))
  }                #sudedam reikalingus duomenis į vieną lentelę
}
NZmet$Kiekis<-as.numeric(NZmet$Kiekis)
NZmet[is.na(NZmet)]<-0
NZmet<-NZmet[-1,]
NZM<-ggplot(NZmet, aes(x=Metai, y=Kiekis, group=Marke))+
  geom_line(aes(color=Marke), size=2) + scale_x_discrete(breaks=seq(2000,2020,2))+ 
  theme(legend.position = "none")+
  ggtitle("N.Zelandija")
NZM
#sujungiam abu grafikus

NZM+AUM+plot_annotation(title="Parduodamų automobilių kiekis pagal išleidimo metus",
                        theme = theme(plot.title=element_text(size = 25, hjust=0.5)))&
                        scale_y_continuous(limits = c(0, 63000))
######################################################################################################



#bandau daryti interaktyvu grafika su ggvis
#sutvarkau duomenis nes yra keistu isskirsciu, taip pat imkime tik sio tukstanmecio masinas
interaktNZ<-subset(NZ,NZ$CC_RATING>500 & NZ$CC_RATING<10000 & !is.na(NZ$GROSS_VEHICLE_MASS), select=c(3, 4, 5, 6))


interaktNZ$BODY_TYPE<-replace(interaktNZ$BODY_TYPE, interaktNZ$BODY_TYPE=="HEAVY VAN" | 
                                interaktNZ$BODY_TYPE=="MOTORCYCLE" |interaktNZ$BODY_TYPE=="OTHER TRUCK" | 
                                interaktNZ$BODY_TYPE=="CAB AND CHASSIS ONLY"| 
                                interaktNZ$BODY_TYPE=="ARTICULATED TRUCK", "Kita")
interaktNZ$BODY_TYPE<-replace(interaktNZ$BODY_TYPE, interaktNZ$BODY_TYPE=="LIGHT VAN" |
                                interaktNZ$BODY_TYPE ==	"FLAT-DECK TRUCK"|
                                interaktNZ$BODY_TYPE ==	"UTILITY", "Daugiatikslis")
interaktNZ$BODY_TYPE<-replace(interaktNZ$BODY_TYPE, interaktNZ$BODY_TYPE=="STATION WAGON", "Universalas")
interaktNZ$BODY_TYPE<-replace(interaktNZ$BODY_TYPE, interaktNZ$BODY_TYPE=="HATCHBACK", "Hečbekas")
interaktNZ$BODY_TYPE<-replace(interaktNZ$BODY_TYPE, interaktNZ$BODY_TYPE=="SALOON", "Sedanas")
interaktNZ$BODY_TYPE<-replace(interaktNZ$BODY_TYPE, interaktNZ$BODY_TYPE=="SPORTS CAR", "Sportinė")
interaktNZ$BODY_TYPE<-replace(interaktNZ$BODY_TYPE, interaktNZ$BODY_TYPE=="CONVERTIBLE", "Kabrioletas")
#Pasirinkime tik 10 populiariausiu markiu duomenis
interaktNZ<-sample_frac(interaktNZ, size = 0.1)
laikNZ<-data.frame(sort(table(interaktNZ$MAKE), decreasing =T))
populNZ<-as.vector(laikNZ[1:5,1])
interaktPopNZ <- interaktNZ[interaktNZ$MAKE %in% populNZ, ]
interaktPopNZ$CC_RATING <- interaktPopNZ$CC_RATING/100 
interaktPopNZ %>%
  ggvis(x=~CC_RATING, y=~GROSS_VEHICLE_MASS, fill = ~BODY_TYPE) %>%
  filter(MAKE %in% eval(input_select(populNZ)) ) %>%
  add_axis("y", title = "Masė", title_offset = 50) %>%
  add_axis("x", title = "Tūris (1=100cm^3)") %>%
  layer_points() %>%
  add_legend( "fill", title = "Kėbulo tipas:")

######################################################################################################




###KODAS GRAFIKAMS KEBULO TIPO PASISKIRSTYMAS

#Naujoji Zelandija

#sukuriamas df (body_type, kiekis, metai)
temp1<-data.frame(matrix(ncol=3,nrow=1))
for(i in 2000:2020){
  a<-subset(NZ, NZ$VEHICLE_YEAR==as.character(i))
  temp<-data.frame(table(a$BODY_TYPE))
  temp$X3<-0
  names(temp)<-c("X1","X2","X3")
  temp$X3<-as.character(i)
  temp1<-rbind(temp1, temp)
}
temp1<-na.omit(temp1)

#visus metus i intervalus
temp1$X3<-replace(temp1$X3, temp1$X3=="2000" | temp1$X3=="2001"|temp1$X3=="2002"|temp1$X3=="2003"|
                    temp1$X3=="2004", "2000-2004")
temp1$X3<-replace(temp1$X3, temp1$X3=="2005" | temp1$X3=="2006"|temp1$X3=="2007"|temp1$X3=="2008"|
                    temp1$X3=="2009", "2005-2009")
temp1$X3<-replace(temp1$X3, temp1$X3=="2010" | temp1$X3=="2011"|temp1$X3=="2012"|temp1$X3=="2013"|
                    temp1$X3=="2014", "2010-2014")
temp1$X3<-replace(temp1$X3, temp1$X3=="2015" | temp1$X3=="2016"|temp1$X3=="2017"|temp1$X3=="2018"|
                    temp1$X3=="2019" | temp1$X3=="2020", "2015-2020")

#idedu i other
temp1$X1<-replace(temp1$X1, temp1$X1=="HEAVY VAN" | temp1$X1=="MOTORCYCLE" |temp1$X1=="OTHER TRUCK" | 
                    temp1$X1=="CAB AND CHASSIS ONLY"| temp1$X1=="ARTICULATED TRUCK", "Other")
temp1$X1<-replace(temp1$X1, temp1$X1=="LIGHT VAN" | temp1$X1 ==	"FLAT-DECK TRUCK", "UTILITY")


#Grafikas naujosios zelandijos pasiskirstymo 
stackedNZ<-ggplot(temp1, aes(x = X3, y = X2, fill = X1)) + geom_bar(stat = 'identity', position = 'fill') +
  labs(y = "%", x = "Metai", fill = "Kėbulo tipas") + theme(legend.position = "none") + 
  scale_y_continuous(labels = scales::percent) + ggtitle("N. Zelandija") + 
  scale_fill_brewer(palette = "Dark2") +
  scale_fill_manual(values = c("brown", "#FF8484", "#00C301", "#10A5F5", "#00DBFF", "#842bd7", "#F99E1F"),
                    labels = c("Kabrioletas", "Hecbekas", "Kita", "Sedanas", "Sportine", "Universalas", "Daugiatikslis"))+ 
  coord_flip()
stackedNZ


###
#Australija
#sukuriamas df (body_type, kiekis, metai)
temp2<-data.frame(matrix(ncol=3,nrow=1))
for(i in 2000:2020){
  a<-subset(AU, AU$`Year of Manufacture`==as.character(i))
  temp<-data.frame(table(a$`Body Shape`))
  temp$X3<-0
  names(temp)<-c("X1","X2","X3")
  temp$X3<-as.character(i)
  temp2<-rbind(temp2, temp)
}
temp2<-na.omit(temp2)


#visus metus i intervalus
temp2$X3<-replace(temp2$X3, temp2$X3=="2000" | temp2$X3=="2001"|temp2$X3=="2002"|temp2$X3=="2003"|
                    temp2$X3=="2004", "2000-2004")
temp2$X3<-replace(temp2$X3, temp2$X3=="2005" | temp2$X3=="2006"|temp2$X3=="2007"|temp2$X3=="2008"|
                    temp2$X3=="2009", "2005-2009")
temp2$X3<-replace(temp2$X3, temp2$X3=="2010" | temp2$X3=="2011"|temp2$X3=="2012"|temp2$X3=="2013"|
                    temp2$X3=="2014", "2010-2014")
temp2$X3<-replace(temp2$X3, temp2$X3=="2015" | temp2$X3=="2016"|temp2$X3=="2017"|temp2$X3=="2018"|
                    temp2$X3=="2019" | temp2$X3=="2020", "2015-2020")


#Ismetineju 
temp2<-subset(temp2, temp2$X1 != "TOWING UNIT UTILITY" & temp2$X1 != "HEAVY AMBULANCE" & temp2$X1 != "CAMPERVAN" &
                temp2$X1 != "TRUK BASED MOTORHOME" & temp2$X1 != "HORSE FLOAT TRUCK" &
                temp2$X1 != "TOW TRUCK TILT TRAY" & temp2$X1 != "TRAY TRUCK" & temp2$X1 !="TRUCK CAB'N'CHASSIS" & 
                temp2$X1 !="PANTECHNICON TRUCK" & temp2$X1 != "TANKER TRUCK" &temp2$X1 != "REFRIGERATED TRUCK"&
                temp2$X1 != "PANTECHNICON VAN" & temp2$X1 !="PLATFORM TRUCK" & temp2$X1!="BUS" & temp2$X1!="REFRIGERATED VAN" &
                temp2$X1 !="TIP TRUCK" & temp2$X1 != "AMBULANCE" & temp2$X1 != "TOWING UNIT UTILITY" & 
                temp2$X1 != "TOW TRUCK" )

#Idedu i other
temp2$X1<-replace(temp2$X1,  temp2$X1 == "TOURER" |temp2$X1 == "STRETCH LIMOUSINE"  | temp2$X1 == "UTE CAB'N'CHASSIS"|
                    temp2$X1=="PANEL VAN"| temp2$X1 == "SOFT TOP" |
                    temp2$X1=="MINIBUS" | temp2$X1 == "TRUCK", "Other")

#sujungiu
temp2$X1<-replace(temp2$X1, temp2$X1 == "WAGON", "STATION WAGON")
temp2$X1<-replace(temp2$X1, temp2$X1 == "SEDAN", "SALOON")
temp2$X1<-replace(temp2$X1, temp2$X1 == "COUPE" | temp2$X1 == "ROADSTER", "SPORTS CAR")
temp2$X1<-replace(temp2$X1, temp2$X1 == "DUAL CAB" | temp2$X1 == "VAN" | temp2$X1 == "MINIBUS", "UTILITY")

#Australijos grafikas pasiskirstymo
stackedAU<-ggplot(temp2, aes(x = X3, y = X2, fill = X1)) + geom_bar(stat = 'identity', position = 'fill') + 
  labs(y = "%", x = "Metai", fill = "Kėbulo tipas") + scale_y_continuous(labels = scales::percent) +
  ggtitle("Australija") +
  scale_fill_brewer(palette = "Dark2")+
  scale_fill_manual(values = c("brown", "#FF8484", "#00C301", "#10A5F5", "#00DBFF", "#842bd7", "#F99E1F"), 
                    labels = c("Kabrioletas", "Hečbekas", "Kita", "Sedanas",
                               "Sportinė", "Universalas", "Daugiatikslis"))+
  theme(axis.text.y = element_blank(), axis.title.y = element_blank(), axis.ticks.y = element_blank()) +
  coord_flip()
stackedAU


stackedNZ + stackedAU + plot_layout(guides = "collect") +
  plot_annotation(title = "Kėbulo tipų pasiskirstymas pagal automobilio metus", 
                  theme = theme(plot.title = element_text(size = 16)))



###############################################################################################################################

############### Populiarusi modeliai



## Naujoji Zelandija
NZ3<-NZ
NZ3<-subset(NZ3, select=c(5, 6, 7, 10))
NZ3$metai<-as.character(NZ3$VEHICLE_YEAR)
NZ3<-transform(NZ3, K_model=paste(NZ3$MODEL, NZ3$metai))
a<-head(sort(table(NZ3$K_model),decreasing = T), n = 10)
data<-data.frame(a)

data$X3<-'a'
data$X4<-0
data$X5<-0
names(data)<- c("Modelis", "Kiekis", "Markė", "Masė", "Metai")
for(i in 1:10){
  b<-subset(NZ3, NZ3$K_model == data[i, 1])
  mase<-mean(b$GROSS_VEHICLE_MASS)
  make<-head(b$MAKE, n = 1)
  year<-head(b$VEHICLE_YEAR, n =1)
  data[i, 3]<-make
  data[i, 4]<-mase
  data[i, 5]<-year
}
#grafikas
library(ggrepel)

test<-ggplot(data, aes(x = Metai, y = Masė, label = Modelis)) + geom_point(aes(colour = Markė, size = Kiekis)) + 
  scale_size_continuous(range = c(5, 9))
test +  geom_text_repel(data = subset(data, data$Masė>2400), nudge_y = 1800 - subset(data, data$Masė>2400)$Metai,
                        force = 35, box.padding = 1.5) + 
  geom_text_repel(data = subset(data, data$Masė<2400), nudge_x = 2004 - subset(data, data$Masė<2400)$Metai,
                  force = 15, nudge_y = 2090-subset(data, data$Masė < 2400)$Metai, size = 4, box.padding = 0.5) +
  ggtitle("N. Zelandijos populiariausi modeliai") + theme_bw()


##Australija
AU3<-AU
AU3$`GVM Weight`<-replace(AU3$`GVM Weight` ,is.na(AU3$`GVM Weight`), 2000)
AU3<-subset(AU3, !is.na(AU3$`GVM Weight`),select=c(5,6,7,12))
AU3$metai<-as.character(AU3$`Year of Manufacture`)
AU3<-transform(AU3, K_model=paste(AU3$Model, AU3$metai))
a<-head(sort(table(AU3$K_model),decreasing = T), n = 10)
table(b$GVM.Weight)
data<-data.frame(a)
data$X3<-'a'
data$X4<-0
data$X5<-0
names(data)<- c("Modelis", "Kiekis", "Markė", "Masė", "Metai")
for(i in 1:10){
  b<-subset(AU3, AU3$K_model == data[i, 1])
  mase<-mean(b$`GVM.Weight`)
  make<-head(b$Make, n = 1)
  year<-head(b$`Year.of.Manufacture`, n =1)
  data[i, 3]<-make
  data[i, 4]<-mase
  data[i, 5]<-year
}
#grafikas
library(ggrepel)
test<-ggplot(data, aes(x = Metai, y = Masė, label = Modelis)) + geom_point(aes(colour = Markė, size = Kiekis)) + 
  scale_size_continuous(range = c(5, 9))
test +  geom_text_repel(data = subset(data, data$Masė>2000), nudge_y = 2020 - subset(data, data$Masė>2000)$Metai,
                        force = 35, box.padding = 1.5) + 
  geom_text_repel(data = subset(data, data$Masė<2000), nudge_x = 2010 - subset(data, data$Masė<2000)$Metai,
                  force = 35, nudge_y = 1800-subset(data, data$Masė < 2000)$Masė, size = 4, box.padding = 1.5) +
  ggtitle("Australijos populiariausi modeliai") + theme_bw()

