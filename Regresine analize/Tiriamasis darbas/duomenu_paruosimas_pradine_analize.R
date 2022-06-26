
Sys.setlocale("LC_ALL","Lithuanian")
library(ggplot2)
library(dplyr)
library("eeptools")
library("lubridate") 
library(ggcorrplot)
library(corrplot)
library(grid)
library(gridExtra)
library(tidyverse)

dataset <- read.csv('HRDataset_v14.csv')

dataset$DOB <- as.Date(dataset$DOB,format="%m/%d/%y") %>% format("19%y%m%d") %>% as.Date("%Y%m%d")
dataset$DateofHire <- as.Date(dataset$DateofHire, format="%m/%d/%Y")
dataset$DateofTermination <- as.Date(dataset$DateofTermination, format="%m/%d/%Y")
dataset$LastPerformanceReview_Date <- as.Date(dataset$LastPerformanceReview_Date, format="%m/%d/%Y")
dataset$Position[dataset$Position == "Data Analyst "] <- "Data Analyst" 

dataset$HispanicLatino <-ifelse(dataset$HispanicLatino == "yes","Yes",
                             ifelse(dataset$HispanicLatino == "no", "No",
                                    dataset$HispanicLatino))

# skaičiuojam asmens amžių
date_today <-  Sys.Date()
x_age <- age_calc(dataset$DOB, date_today, units = "years")
dataset$Age <- floor(x_age) 

# kiek laiko išdirbo mėnesiais
dirbo_men <-ifelse(is.na(dataset$DateofTermination), 
                         age_calc(dataset$DateofHire, date_today, units = "months"),
                         ifelse(!is.na(dataset$DateofTermination), 
                                interval(dataset$DateofHire, 
                                         dataset$DateofTermination) %/% months(1), NA))
dataset$dirbo_men <- floor(dirbo_men)

dataset$State <- ifelse(dataset$State == "MA", "MA", "Other")

dataset$CitizenDesc <- ifelse(dataset$CitizenDesc == "US Citizen", "US", "Other")

dataset$RecruitmentSource <- ifelse(dataset$RecruitmentSource ==
                                   "On-line Web application", "Website",
                                 dataset$RecruitmentSource)
dataset$RecruitmentSource <- ifelse(dataset$RecruitmentSource == "Other", 
                                    "Indeed", dataset$RecruitmentSource)


dataset$RaceDesc <- ifelse(dataset$RaceDesc=='Hispanic',
                           'Black or African American',dataset$RaceDesc)
dataset$RaceDesc <- ifelse(dataset$RaceDesc=='American Indian or Alaska Native',
                        'Two or more races',dataset$RaceDesc)

# paliksime 5 didziausius ir "kita"
data1 <- dataset %>%
  mutate(TermReason = fct_lump(TermReason, n = 5, other_level = "Other"))

data1 <- data1[data1$Department!="Executive Office",]
#-------------------------------------------------------------------------------
colnames(data1)
data <- data1[-c(1,2,3,4,5,6,7,8,12,15,16,22,23,25,27,28,34)]
colnames(data)
data[c(1,3:14)] <- lapply(data[c(1,3:14)], factor)


# Position apjungimas-----------------------------------------------------------

junior <- list('Accountant I','Administrative Assistant',
               'IT Support','Network Engineer','Production Technician I')

mid <- list('BI Developer','Database Administrator','Enterprise Architect',
            'Data Analyst','Production Technician II','Software Engineer')

senior <- list('Sr. Accountant', 'Sr. DBA','Sr. Network Engineer',
               'Senior BI Developer','Data Architect','Principal Data Architect')

manager <- list('Software Engineering Manager','Shared Services Manager',
                'Sales Manager','Production Manager','IT Manager - Infra',
                'IT Manager - DB','IT Manager - Support','Area Sales Manager')

director <- list('IT Director','Director of Sales','Director of Operations',
                 'BI Director','CIO','President & CEO')


data$Position_merged[data$Position %in% junior] <- "Junior"
data$Position_merged[data$Position %in% mid] <- "Mid-level"
data$Position_merged[data$Position %in% senior] <- "Senior"
data$Position_merged[data$Position %in% manager] <- "Manager"
data$Position_merged[data$Position %in% director] <- "Director"

data$Position_merged <- as.factor(data$Position_merged)

tapply(data$Salary, data$Position_merged, summary) 
summary(data$Position_merged)

#-------------------------------------------------------------------------------

summary(data$TermReason)
summary(data$Termd)
summary(data$RecruitmentSource)
summary(data$Department)
summary(data$CitizenDesc)
summary(data$HispanicLatino)
summary(data$TermReason)
summary(data$CitizenDesc)
summary(data$HispanicLatino)
summary(data$Position_merged)
summary(data$RaceDesc)

#write.csv(data, file = "HRDataset.csv", row.names = F)

# Pirminė duomenų analizė ------------------------------------------------------

p01 <- ggplot(data, aes(x=Sex, y=Salary, fill=Sex)) + 
  geom_boxplot(alpha=0.4) +
  theme(legend.position="none") +
  scale_fill_brewer(palette="BuPu") +
  labs(title= "Atlyginimų pasiskirstymas pagal lytį", 
       x = "Lytis", y = "Atlyginimas") + 
  scale_x_discrete(labels=c("Moteris","Vyras")) 
p02 <- ggplot(data, aes(x=MaritalDesc, y=Salary, fill=MaritalDesc)) + 
  geom_boxplot(alpha=0.4) +
  theme(legend.position="none") +
  scale_fill_brewer(palette="BuPu") + 
  labs(title= "Atlyginimų pasiskirstymas pagal šeimyninę padėtį", 
       x = "Šeimyninė padėtis", y = "Atlyginimas") + 
  scale_x_discrete(labels=c("Išsiskyręs","Vedęs","Gyvena atskirai","Vienišas","Našlys"))
grid.arrange(p01, p02, nrow = 1, widths=c(2.3, 3))


p03 <- ggplot(data, aes(x=Department, y=Salary, fill=Department)) + 
  geom_boxplot(alpha=0.4) +
  theme(legend.position="none") +
  scale_fill_brewer(palette="BuPu") + 
  labs(title= "Atlyginimai pagal departamentą", 
       x = "Departamentas", y = "Atlyginimas") +
  scale_x_discrete(labels=c("Administracija","Valdyba","IT/IS","Gamyba",
                            "Pardavimai","Programinės įrangos\n inžinerija"))
p03
summary(data$Department)


p04 <- ggplot(data, aes(x=Department, y=Age, fill=Department)) + 
  geom_boxplot(alpha=0.4) +
  theme(legend.position="none") +
  scale_fill_brewer(palette="BuPu") + 
  labs(title= "Amžiaus pagal departamentą", 
       x = "Departamentas", y = "Amžius") + 
  scale_x_discrete(labels=c("Administracija","Valdyba","IT/IS","Gamyba",
                            "Pardavimai","Programinės įrangos\n inžinerija"))
p04


ggplot(data, aes(Salary)) +
  geom_bar() + scale_x_binned() + labs(title= "Atlyginimų pasiskirstymas", 
                                       x = "Atlyginimas", y = "Dažnis")
colnames(data)
# koreliacijų matrica
d<-cor(data[c(2,15:21)])
d
colnames(d) <- c("Atlyginimas", "Įsitraukimas", "Pasitenkinimas","Specialių projektų \n skaičius",
                    "Vėlavimas", "Praleistų dienų\n skaičius","Amžius","Darbo laikas\n mėnesiais")
rownames(d) <- c("Atlyginimas", "Įsitraukimas", "Pasitenkinimas","Specialių projektų \n skaičius",
                 "Vėlavimas", "Praleistų dienų\n skaičius","Amžius","Darbo laikas\n mėnesiais")
ggcorrplot(d, title = "Koreliacijų matrica",lab = TRUE)



