library(tidyverse)
df<- diamonds

# 1-as žingsnis: atrenkame stulpelius x,y,z (0,1)
subDf <- df[c("x","y","z")]

# 2-as žingsnis: pervadiname atrinktus stulpelius (0,1)
names(subDf) <- c("X","Y","Z")

# 3-ias žingsnis: sukuriame naują stulpelį (0,2)
subDf$prod <- apply(X=subDf,MARGIN = 1, 
                    FUN = function(r){
                      p <- 1
                      for(i in r){
                        p <- p*i 
                      }
                      if (p>40){return(p)} else {return(30)}
                    })

# 4-as žingsnis: atrenkame lygines eilutes (0,2)
subDf <- subDf[seq(2,nrow(subDf),2),]

# 5-as žingsnis apskaičiuojame suvestines (0,2)
c(mean(subDf$X),var(subDf$Y),
  min(subDf$Z),mean(abs(subDf$prod-mean(subDf$prod))))
