# Duomenų nuskaitymas

library(tidyverse)
library(car)
library(ggpubr)


data = as.data.frame(read.table('S1File.txt', header = TRUE))
data$apl <- as.factor(data$apl)
colnames(data)[1] <- 'age'
head(data)


# Vidurkių grafiko braižymas 

error_plot <- function(df, factor, variable, xlabel, ylabel, title){
  
  ggerrorplot(df, x = factor, y = variable, 
              desc_stat = "mean_sd",
              error.plot = "errorbar",
              add = "mean") +
    xlab(xlabel) + ylab(ylabel) + ggtitle(title)+
    theme_bw()
  
}


# Vienfaktorė dispersinė analizė (ūgis)

heights <- filter(data, hei < 300, hei > 0)

error_plot(heights, 'apl', 'hei', 'Grupė', 'Ūgis (cm)','Ūgio vidurkiai grupėse')

leveneTest(hei ~ apl, data=heights, center = median)

anova.height <- aov(hei ~ apl, data=heights)
summary(anova.height)

heights %>%
  group_by(apl) %>%
  dplyr::summarize(Mean = mean(hei, na.rm=TRUE), SD=sd(hei,na.rm=TRUE))

pairwise.t.test(heights$hei, heights$apl, p.adj = "bonf")


tukey.rez <- TukeyHSD(anova.height)
plot(tukey.rez)


#-------------------------------------------------------------------------

# Vienfaktorė dispersinė analizė (masė)

weights <- filter(data, wei < 80 , wei > 0)

error_plot(weights, 'apl', 'wei', 'Grupė', 'Masė (kg)', 'Masės vidurkiai grupėse')

leveneTest(wei ~ apl, data=weights, center=median)

anova.weight <- aov(wei ~ apl, data=weights);
summary(anova.weight)


weights %>%
  group_by(apl) %>%
  dplyr::summarize(Mean = mean(wei, na.rm=TRUE), SD=sd(wei,na.rm=TRUE))

pairwise.t.test(weights$wei, weights$apl, p.adj = "bonf")


tukey.rez <- TukeyHSD(anova.weight)
plot(tukey.rez)


#-------------------------------------------------------------------------


# Vienfaktorė dispersinė analizė (sprintas)

sprints <- filter(data, spr != 999 , spr > 0)

error_plot(sprints, 'apl', 'spr', 'Grupė', 'Laikas (s)', 'Sprintas')

leveneTest(spr ~ apl, data=sprints, center=median)

anova.sprint <- aov(spr ~ apl, data=sprints);
summary(anova.sprint)


sprints %>%
  group_by(apl) %>%
  dplyr::summarize(Mean = mean(spr, na.rm=TRUE), SD=sd(spr,na.rm=TRUE))

pairwise.t.test(sprints$spr, sprints$apl, p.adj = "bonf")

tukey.rez <- TukeyHSD(anova.sprint)
plot(tukey.rez)


#-------------------------------------------------------------------------


# Vienfaktorė dispersinė analizė (vikrumas)

agility <- filter(data, agi != 999 , agi > 0)

error_plot(agility, 'apl', 'agi', 'Grupė', 'Laikas (s)', 'Vikrumas')

leveneTest(agi ~ apl, data=agility, center=median)

oneway.test(agi ~ apl, data = agility, var.equal = FALSE)

agility %>%
  group_by(apl) %>%
  dplyr::summarize(Mean = mean(agi, na.rm=TRUE), SD=sd(agi,na.rm=TRUE))

pairwise.t.test(agility$agi, agility$apl, p.adj = "bonf")


#-------------------------------------------------------------------------


# Vienfaktorė dispersinė analizė (driblingas)

dribbling <- filter(data, dri != 999 , dri > 0)

error_plot(dribbling, 'apl', 'dri', 'Grupė', 'Laikas (s)', 'Driblingas')

leveneTest(dri ~ apl, data=dribbling, center=median)

oneway.test(dri ~ apl, data = dribbling, var.equal = FALSE)

dribbling %>%
  group_by(apl) %>%
  dplyr::summarize(Mean = mean(dri, na.rm=TRUE), SD=sd(dri,na.rm=TRUE))

pairwise.t.test(dribbling$dri, dribbling$apl, p.adj = "bonf")

#-------------------------------------------------------------------------


# Vienfaktorė dispersinė analizė (kamuolio kontrolė)

ball_control <- filter(data, bc != 999 , bc > 0)

error_plot(ball_control, 'apl', 'bc', 'Grupė', 'Laikas (s)', 'Kamuolio kontrolė')

leveneTest(bc ~ apl, data=ball_control, center=median)

anova.ball_control <- aov(bc ~ apl, data=ball_control);
summary(anova.ball_control)

ball_control %>%
  group_by(apl) %>%
  dplyr::summarize(Mean = mean(bc, na.rm=TRUE), SD=sd(bc,na.rm=TRUE))

pairwise.t.test(ball_control$bc, ball_control$apl, p.adj = "bonf")

tukey.rez <- TukeyHSD(anova.ball_control)
plot(tukey.rez)

#-------------------------------------------------------------------------


# Vienfaktorė dispersinė analizė (kamuolio smūgiavimas)

shooting <- filter(data, sho != 999 , sho > 0)

error_plot(shooting, 'apl', 'sho', 'Grupė', 'Taškai', 'Kamuolio smūgiavimas')

leveneTest(sho ~ apl, data=shooting, center=median)

oneway.test(sho ~ apl, data = shooting, var.equal = FALSE)


shooting %>%
  group_by(apl) %>%
  dplyr::summarize(Mean = mean(sho, na.rm=TRUE), SD=sd(sho,na.rm=TRUE))

pairwise.t.test(shooting$sho, shooting$apl, p.adj = "bonf")

#############################################################################
#############################################################################
