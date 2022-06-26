#1. Savo pavyzdþiu iliustruokite nekryptinio tinklo sukûrimà 
#naudojant funkcijà graph_from_literal, paaiðkinkite funkcijos 
#sintaksæ. Raskite tinklo eilæ ir dydá. Panaudokite funkcijà plot
#tinklo vizualizavimui. Paaiðkinkite funkcijos print_all pateikiamà 
#rezultatà.

library(igraph)
g<-graph_from_literal(1-5,4-5,1-3,3-6,2-4,4-6,7)
plot(g)
#grafp dydis - briaunu skaicius
degree(g)#kiekvienai
E(g)
#grafoveile - virsuniu skaicius
V(g)
print_all(g)

#2. Savo pavyzdþiu iliustruokite kryptinio tinklo sukûrimà naudojant 
#funkcijà graph_from_literal, paaiðkinkite funkcijos sintaksæ. 
#Raskite tinklo eilæ ir dydá. Panaudokite funkcijà plot tinklo 
#vizualizavimui. Paaiðkinkite funkcijos print_all pateikiamà 
#rezultatà. Naudodami funkcijà V priskirkite virðûnëms 
#pavadinimus ¥V1´, ¥V2´, ... 

g2<-graph_from_literal(1-+5,4-+5,1-+3,3-+6,2-+4,4-+6,7)
plot(g2)
V(g2)
E(g2)
V(g2)$name<-c("A","B","C","D","E","F","G")
plot(g2)

#3. Sukurkite kryptiná ir nekryptiná grafà naudodami gretimumo sàraðà, briaunø sàraðà ir gretimumo matricà.
netmat1 <- rbind(c(0,1,1,0,0),
                 c(0,0,1,1,0),
                 c(0,1,0,0,0),
                 c(0,0,0,0,0),
                 c(0,0,1,0,0))
rownames(netmat1) <- c("A","B","C","D","E")
colnames(netmat1) <- c("A","B","C","D","E")
netmat1#pasidarome  matrica
is.matrix(netmat1)

g3<-graph_from_adjacency_matrix(netmat1);g3
plot(g3)


netmat2 <- rbind(c(1,2),
                 c(1,3),
                 c(2,3),
                 c(2,4),
                 c(3,2),
                 c(5,3))
netmat2
g4<-graph_from_edgelist(netmat2)
plot(g4)

#bet geriau sito nenaudoti
netmat3<-as_adj_list(g4)
netmat3
g5<-graph_from_adj_list(netmat3)
plot(g5)


#4. Sukurkite indukuotà grafikà ið pirmame punkto sukurto.

g5<-induced_subgraph(g,1:5)
print_all(g5)
plot(g5)
#5. Raskite tam tikros virðûnës kaimynus.
neighbors(g,3)

#6. Sukurkite grafà, kuriame bûtø ávairaus tipo keliø.
# (Zinoti kas yra kelias, kas yra ciklas)