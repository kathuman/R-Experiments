# Social Network Analysis
setwd("C:/Users/kathuman/Dropbox/DTU PhD/Preliminary research/R Programs/Social Network Analysis")
rm(list=ls())

# load termDocMatrix
load("termDocMatrix.rdata")
# inspect part of the matrix
termDocMatrix[5:10,1:20]
dim(termDocMatrix)
termDocMatrix <- as.matrix(termDocMatrix)

# change it to a Boolean matrix
termDocMatrix[termDocMatrix>=1] <- 1
# transform into a term-term adjacency matrix
termMatrix <- termDocMatrix %*% t(termDocMatrix)
# inspect terms numbered 5 to 10
termMatrix[5:10,5:10]

## Now build the graph
install.packages("igraph")
library(igraph)
library(help=igraph)
install.packages(c("sna", "network", "statnet"))
install.packages("statnet")
library(sna)
install.packages("slam")
library(help=sna)
library(network)
library(help=network)
library(statnet)
library(help=statnet)
# build a graph from the above matrix
g <- graph.adjacency(termMatrix, weighted=T, mode = "undirected")
close <-as.data.frame(closeness(g))

# remove loops
g <- simplify(g)
# set labels and degrees of vertices
V(g)$label <- V(g)$name
V(g)$degree <- degree(g)

# set seed to make the layout reproducible
set.seed(3952)
layout1 <- layout.fruchterman.reingold(g)
plot(g, layout=layout1)

plot(g, layout=layout.kamada.kawai)
tkplot(g, layout=layout.kamada.kawai)

## a better way of representing it
V(g)$label.cex <- 2.2 * V(g)$degree / max(V(g)$degree)+ .2
V(g)$label.color <- rgb(0, 0, .2, .8)
V(g)$frame.color <- NA
egam <- (log(E(g)$weight)+.4) / max(log(E(g)$weight)+.4)
E(g)$color <- rgb(.5, .5, 0, egam)
E(g)$width <- egam
# plot the graph in layout1
plot(g, layout=layout1)

