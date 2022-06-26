library(readr)
library(igraph)

#nuskaitomi duomenys
erasmus <- read_csv("erasmus.csv")
data<-subset(erasmus, select = c(2, 4, 5, 6))
data<-data[2:21, ]

#nubreziamas neorientuotas, nesvorinis grafas (g1)
temp<-subset(data, select=c(1,2))
g1<-graph_from_data_frame(temp, directed = F)
plot(g1)

#grafa verciame i orientuota grafa (g2)
g2<-as.directed(g1, mode = c("mutual", "arbitrary"))
plot(g2, edge.curved=.3)

#nesvorini grafa verciame svoriniu grafu
data1<-as_data_frame(g2)
data1$weight<-as.numeric(c(data$X5, data$Students))
g3<-graph_from_data_frame(data1, directed = T)

# pakeiciame, kad briaunu plociai butu proporcingi svoriams, 
# o virsuniu dydziai priklauso nuo stiprumo (ateinanciu briaunu)
E(g3)$width<-E(g3)$weight*0.02
V(g3)$size<-sqrt(strength(g3, mode= "in"))
plot(g3, edge.curved=0.4, layout=layout_with_dh, edge.arrow.size=1.2, edge.color="darkblue")

print_all(g3) #visa info apie grafa (DNW-  directed, named, weighted, be specialiu virsuniu)
V(g3) #virsunes
E(g3) #briaunos
get.vertex.attribute(g3) #virsuniu atributai
get.edge.attribute(g3)   #briaunu atributai
igraph::degree(g3, mode="in") #ateinanciu virsuniu laipsnis 
strength(g3) #virsuniu stiprumas
is.simple(g3) # paprastasis grafas
is.connected(g3) # ar visos virsunes sujungtos

# siuo atveju diametras nelabai turi prasmes
diameter(g3) 
get_diameter(g3)

# visuniu laipsniu histograma
hist(igraph::degree(g3), col="lightblue", xlim=c(0,20), ylim = c(0,4), breaks = seq(0, 20, 2),
     xlab="Virsuniu laipsniai", ylab="Daznis", main="Virsuniu laispniu dazniai", xaxp = c(0, 20, 10))
par(xpd=F)
abline(v=mean(igraph::degree(g3)), lwd = 5, lty=2)

#virsuniu stiprumo diagrama (ateinanciu briaunu)
hist(strength(g3), col="pink",
     xlab="Virsuniu stiprumas", ylab="Dazniai", main="Virsuniu stiprumu dazniai")
par(xpd=F)
abline(v=mean(strength(g3)), lwd = 5, lty=2)