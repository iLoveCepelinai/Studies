Sys.setlocale("LC_ALL","Lithuanian")

library(data.table)
library(lubridate)
library(tidyverse)
library(corrplot)

duom<-as.data.frame(fread("galDuom.csv"))
duom$menuo <- as.factor(month(ymd(duom$data)))

############################################
# APRASOMOJI STATISTIKA
############################################
library(psych)
A <- describe(duom[,2:9], skew = F, quant = c(0.25, 0.75))
A <- A[,c(3:7,9,10)]
options(scipen = 999)
htmlTable::htmlTable(format(round(A,3)))

############################################
# TASKINES DIAGRAMOS
############################################

# SAULE
(p1 <- ggplot(duom, aes(x=temp, y=kwh, color = menuo)) +
  geom_point(size = 5, alpha = 0.8) +
  theme_bw() + ggtitle("Temperatūros") +
  xlab("Temperatūra (°C)") + ylab("Energija") +
  scale_colour_discrete("Mėnuo") + theme(plot.title = element_text(size=12)))

(p2 <- ggplot(duom, aes(x=IR, y=kwh, color = menuo)) +
  geom_point(size = 5, alpha = 0.8) +
  theme_bw()+ ggtitle("IR spindulių") +
  xlab("IR") + ylab("") +
  scale_colour_discrete("Mėnuo") +
  theme(plot.title = element_text(size=12), axis.text.y=element_blank()))

(p3 <- ggplot(duom, aes(x=d_ilg, y=kwh, color = menuo)) +
  geom_point(size = 5, alpha = 0.8) +
  theme_bw() + ggtitle("Dienos ilgio") +
  xlab("Dienos ilgis (h)") + ylab("Energija") +
  scale_colour_discrete("Mėnuo") + theme(plot.title = element_text(size=12)))

(p4 <- ggplot(duom, aes(x=irr, y=kwh, color = menuo)) +
  geom_point(size = 5, alpha = 0.8) +
  theme_bw() + ggtitle("Spinduliuotės") +
  xlab("Spinduliuotė") + ylab("") +
  scale_colour_discrete("Mėnuo") +
    theme(plot.title = element_text(size=12), axis.text.y=element_blank()))

library(patchwork)
p1234 <- p1 + p2 + p3 + p4
p1234 + plot_layout(ncol = 2, guides = "collect") +
  plot_annotation(title = "Energijos priklausomybė nuo:", tag_levels = 'I') &
  scale_y_continuous(limits = c(0, 0.15)) & 
  theme(plot.tag = element_text(size = 12))

# Matome, kad yra 2 labai issiskiriancios reiksmes. Didziausia problema del 
# 2019 09 14 dienos, kai spinduliuote buvo didele, bet kwh - labai maza. 
# Automatiskai susigadina ir 09 15 duomenys, nes skirtumas isskirtinai didelis.
ggplot(duom[-c(14,15),], aes(x=irr, y=kwh, color = menuo)) +
  geom_point(size = 5, alpha = 0.8) +
  theme_bw() + ggtitle("Energijos priklausomybė nuo spinduliuotės") +
  xlab("Spinduliuotė") + ylab("Energija") +
  scale_colour_discrete("Mėnuo")



# VEJAS
ggplot(duom, aes(x=v_gr, y=kwh)) +
  geom_point(size = 5, alpha = 0.9, color = "#A8A6DB") +
  theme_bw()+
  geom_smooth(method=lm) + 
  ggtitle("Energijos priklausomybė nuo vėjo greičio") + 
  xlab("Vėjo greitis (km/h)") + ylab("Energija")


# KRITULIAI
ggplot(duom, aes(x=krituliai, y=kwh)) +
  geom_point(color = '#C8EAEB', size = 5, alpha = 0.9) +
  theme_bw()+
  geom_smooth(method=lm) +
  ggtitle("Energijos priklausomybė nuo kritulių") +
  xlab("Krituliai (mm)") + ylab("Energija")


# SLEGIS
ggplot(duom, aes(x=slegis, y=kwh)) +
  geom_point(size = 5, alpha = 0.9, color = '#A6D2E1') +
  theme_bw() +
  ggtitle("Energijos priklausomybė nuo slėgio") +
  xlab("Slėgis (hPa)") + ylab("Energija") +
  scale_colour_discrete("Mėnuo") +
  geom_smooth(method=lm)

########## BOXPLOT ##########

# MENUO
(p21 <- ggplot(duom, aes(x=menuo, y=kwh, fill = menuo)) +
  theme_bw()+
  geom_boxplot() + theme(legend.position="none") +
  ggtitle("Energijos") +
  xlab("Mėnuo") + ylab("Energija"))

(p22 <- ggplot(duom, aes(x=menuo, y=temp, fill = menuo)) +
  theme_bw()+
  geom_boxplot() + theme(legend.position="none") +
  ggtitle("Temperatūros") +
  xlab("Mėnuo") + ylab("Temperatūra (°C)"))

(p23 <- ggplot(duom, aes(x=menuo, y=irr, fill = menuo)) +
  theme_bw()+
  geom_boxplot() + theme(legend.position="none") +
  ggtitle("Spinduliuotės") +
  xlab("Mėnuo") + ylab("Spnduliuotė"))

(p24 <- ggplot(duom, aes(x=menuo, y=IR, fill = menuo)) +
  theme_bw()+
  geom_boxplot() + theme(legend.position="none") +
  ggtitle("IR") +
  xlab("Mėnuo") + ylab("IR"))

(p25 <- p21 + p22 + p23 + p24  +
  plot_annotation(title = "Pasiskirstymas metuose priklausomai nuo:",
                  tag_levels = 'I',
caption="Pastaba: parinktos tik didžiausių koreliacijų kovariantės ir priklausoma kovariantė")&
  theme(plot.tag = element_text(size = 12)))

############################################
# TANKIS
############################################
library(ggridges)

ggplot(duom, aes(x = kwh, y = menuo, fill = stat(x))) +
  geom_density_ridges_gradient(scale = 3, size = 0.3, rel_min_height = 0.01) +
  scale_fill_viridis_c(name = "Energija", option = "C") +
  labs(title = 'Energijos tankio grafikai pagal mėnesį') +
  xlab("Energija") + ylab("Mėnuo")

############################################
# HISTOGRAMOS
############################################
duom20 <- duom[as.factor(year(ymd(duom$data)))=='2020',]

ggplot(duom20, aes(x=kwh, fill = menuo)) +
  geom_histogram(bins = 15, alpha=0.9, color = "black") +
  theme_bw() +
  xlab("Energija") + ylab("Skaičius") +
  ggtitle("Pagaminamos energijos pasiskirstymas") +
  scale_fill_discrete(name = "Mėnuo")

############################################
# KORELIACINE DIAGRAMA
############################################

M<-cor(duom[,2:9])
corrplot.mixed(M, upper = 'ellipse', title = 'Kintamųjų koreliacijos', 
               mar=c(0,0,2,0), tl.col = 'black', tl.pos = 'd')
