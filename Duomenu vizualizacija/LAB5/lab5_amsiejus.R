#Naudojamos bibliotekos
library(readr)

#Duomenu nuskaitymas, atrinkimas ir tvarkymas
duomenys<-read_csv("Atviri_TP_parko_duomenys.csv")
masinos<-subset(duomenys, duomenys$KATEGORIJA_KLASE=="M1", select = c(1,2,17,21,27,28,31,52,54,60))
masinos$Metai<-strtoi(substring(masinos$PIRM_REG_DATA,1,4))
masinos<-subset(masinos, masinos$MARKE!="Nuasmeninta")

#sukuriame masinos modelio pagal metus kintamaji
masinos<-transform(masinos, modelis=paste(masinos$KOMERCINIS_PAV, masinos$Metai))

#filtruojame 10 populiariausiu markiu lietuvoje
#pirma sukuriame lentele kuri suskaiciuoja visu markiu kieki ir surusiuoja mazejimo tvarka ir paima 10 didziausiu
markes<-data.frame(head(sort(table(masinos$MARKE),decreasing = T), n = 10))
#tada atsiskiriame vektoriu, kuris pades mums filtruoti masinas
popmarkes<-as.vector(markes[,1])
#atrenkame tik tas masinas, kurios yra vienos is 10 populiariausiu markiu lietuvoje
interaktDuom <- masinos[masinos$MARKE %in% popmarkes, ]
interaktDuom <- subset(interaktDuom, !is.na(interaktDuom$Metai)&interaktDuom$GALIA<1000)
#sukuriu nauja lentele kuri jau talpins grafikui paruostus duomenis
galutDuom <- data.frame("Marke" = 'a', "Modelis"='b', "Kiekis"=4, "Metai"=4, "Galia"=4, "Mase"=4, "Benzinas"=6,
                        "Dyzelinas"=2, "EV"=5)
names(galutDuom) <- c("Markė", "Modelis", "Kiekis", "Metai", "Galia", "Masė", "Benzinas", "Dyzelinas", "EV")

#atsispausdinu sau degalu tipus, kad matyciau, pagal ka suskirstyti toliau
table(interaktDuom$DEGALAI)

for(marke in popmarkes){
  #padaro markes esancios populiariausiuju sarase poaibi
  temp1<-subset(interaktDuom, MARKE==marke)
  #is gauto poaibio tik su viena marke randame populiariausiu modeliu lentele
  temp2<-data.frame(head(sort(table(temp1$modelis),decreasing = T), n = 20))
  #atrenkame popualiariausiu modeliu pavadinimus
  popmodeliai<-as.vector(temp2[,1])
  #sukame cikla per populiariausius modelius
  for(modeliukas in popmodeliai){
    #atrenkame tik konkreciu metu konkretu modeli
    temp3<-subset(temp1, temp1$modelis == modeliukas)
    #randame jo marke
    tmarke<-head(temp3$MARKE,n=1)
    #randame jo modeli (originalus, be pridetu metu)
    tmodelis<-head(temp3$KOMERCINIS_PAV, n=1)
    #suskaiciuojame, kiek ju isviso yra
    tkiekis<-nrow(temp3)
    #pasiimame jo metus
    tmetai<-head(temp3$Metai,n=1)
    #suskaiciuojame vidutine galia (tarp skirtingu kompletaciju gali skirtis galios)
    tgalia<-round(mean(temp3$GALIA, na.rm = T),0)
    #suskaiciuojame vidutine mase (tarp skirtingu kompletaciju gali skirtis mases)
    tmase<-round(mean(temp3$NUOSAVA_MASE, na.rm = T),0)
    #randame dali masinu, kuriu pagrindinis kuro tipas yra benzinas (isskyrus elektriniai hibridai)
    tbenz<-round(nrow(subset(temp3, temp3$DEGALAI=="Benzinas"|temp3$DEGALAI=="Benzinas / Dujos"|
                               temp3$DEGALAI=="Benzinas / Etanolis"|temp3$DEGALAI=="Benzinas / Etanolis / Dujos"|
                               temp3$DEGALAI=="Benzinas / Gamtinės_dujos"|
                               temp3$DEGALAI=="Benzinas / Suskystintos_dujos"))/tkiekis*100, 2)
    #randame dali masinu, kuriu pagrindinis kuro tipas yra dyzelinas (isskyrus elektriniai hibridai)
    tdyz<-round(nrow(subset(temp3, temp3$DEGALAI=="Dyzelinas"|temp3$DEGALAI=="Dyzelinas / Dujos"))/tkiekis*100, 2)
    #randame dali masinu, kurios yra elektrines/elektriniai hibridai
    tev<-round(nrow(subset(temp3, temp3$DEGALAI=="Benzinas / Elektra"|temp3$DEGALAI=="Benzinas / Elektra / Dujos"|
                             temp3$DEGALAI=="Dyzelinas / Elektra"|temp3$DEGALAI=="Elektra"))/tkiekis*100, 2)
    #pridedame surinktus duomenis apie modeli prie musu galutines lenteles
    galutDuom[nrow(galutDuom) + 1,] <- c(tmarke, tmodelis, tkiekis, tmetai, tgalia, tmase, tbenz, tdyz, tev)
  }
}

#Kadangi mano lenteleje yra kai kurie skaiciu stulpeliai charactrer ir pirmas stulpelis yra nereikalingas, tvarkome
#istriname pirma eilute
galutDuom = galutDuom[-1,]
#konvertuojame i numeric
cols_num <- c("Kiekis","Metai","Galia","Masė","Benzinas","Dyzelinas","EV")
galutDuom[cols_num] <- sapply(galutDuom[cols_num],as.numeric)


####################################################3
#grafiko piesimas
library(plotly)
#piesiame grafika pagal metus ir galia. Filtras bus marke, o uzvedus pele rasys daug papildomos informacijos
fig <- plot_ly(galutDuom, x = ~Metai, y = ~Galia, type = 'scatter', mode = 'markers', size = ~Kiekis, color = ~Markė,
               colors = 'Paired', sizes = c(10, 50), marker = list(opacity = 0.7, sizemode = 'diameter'),
               hoverinfo = 'text',
               text = ~paste('Markė:', Markė, '<br>Modelis:', Modelis, '<br>Kiekis:', Kiekis, '<br>Metai:', Metai,
                             '<br>Vidutinė masė:', Masė,
                             '<br>Kuro pasiskirstymas(%):','<br>Benzinas:',Benzinas,'<br>Dyzelinas:',Dyzelinas,
                             '<br>EV:',EV))
#pridedame grafiko ir legendos pavadinima
fig <- fig %>% layout(title = 'Populiariausių modelių pasiskirstymas pagal metus ir galią',
                      legend=list(title=list(text='<b> Markė </b>')))
fig
