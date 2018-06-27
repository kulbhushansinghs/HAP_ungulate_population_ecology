## set path
library(igraph)
setwd("/Users/Kullu/Dropbox (Snow Leopard Trust)/Kullu_desktop/Git/HAP_ungulate_population_ecology/Nilgiri Tahr/renilgiritahrinformation")

dat<- read.csv("meta pop.csv")
distance<-read.csv("Meta distance.csv")

dat
m<-as.matrix(dat[1:12, 2:13])
net=graph.adjacency(m,mode="directed",weighted=T,diag=FALSE) 
plot.igraph(net,vertex.label=V(net)$name, edge.arrow.size = 0.5, size2 = c(100,1,1,1,1,1,1,1,1,1,1,1))

V(net)$name <- c("Grass hills", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L")

E(net)$weight <- distance[1:12, 2:13]
