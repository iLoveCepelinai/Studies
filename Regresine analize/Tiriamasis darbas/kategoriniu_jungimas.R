library(tidyverse)
data <- read.csv("HRDataset.csv")

table(data$TermReason)

# State
table(data$State)
data$State <- ifelse(data$State == "MA", "MA", "Other")

# US citizen
table(data$CitizenDesc)
data$CitizenDesc <- ifelse(data$CitizenDesc == "US Citizen", "US", "Other")

# HispanicLatino
table(data$HispanicLatino)
data$HispanicLatino <-ifelse(data$HispanicLatino == "yes","Yes",
                          ifelse(data$HispanicLatino == "no", "No",
                                 data$HispanicLatino))

# EmploymentStatus neimame !!!!

# Department
table(data$Department)
data <- data[data$Department!="Executive Office",]

# RecruitmentSource
table(data$RecruitmentSource)
data$RecruitmentSource <- ifelse(data$RecruitmentSource ==
                                   "On-line Web application", "Website",
                                 data$RecruitmentSource)
data <- data[data$RecruitmentSource!="Other",]

# Position_merged
table(data$Position_merged)
data$Position_merged <- ifelse(data$Position_merged == "Chief", "Director", 
                           data$Position_merged)

# TermReason
sort(table(data$TermReason))

# paliksime 5 didziausius ir "kita"
data1 <- data %>%
  mutate(TermReason = fct_lump(TermReason, n = 5, other_level = "Other"))