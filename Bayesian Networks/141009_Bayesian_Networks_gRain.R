install.packages("gRain")
install.packages("RBGL")
library(gRain)
library(RBGL)

# Specify conditional probability tables
yn <- c("yes","no")
a <- cptable(~asia, values=c(1,99),levels=yn)
t.a <- cptable(~tub|asia, values=c(5,95,1,99),levels=yn)
s <- cptable(~smoke, values=c(5,5), levels=yn)
l.s <- cptable(~lung|smoke, values=c(1,9,1,99), levels=yn)
b.s <- cptable(~bronc|smoke, values=c(6,4,3,7), levels=yn)
e.lt <- cptable(~either|lung:tub,values=c(1,0,1,0,1,0,0,1),levels=yn)
x.e <- cptable(~xray|either, values=c(98,2,5,95), levels=yn)
d.be <- cptable(~dysp|bronc:either, values=c(9,1,7,3,8,2,1,9), levels=yn)

# Compile list of conditional probability tables and create the network
plist <- compileCPT(list(a, t.a, s, l.s, b.s, e.lt, x.e, d.be))
plist

plist$tub

plist$either ## Notice: a logical node

net1 <- grain(plist)
net1

# Querying a network
# Marginal Probabilities
querygrain(net1, nodes=c("lung","bronc"), type="marginal")

#  joint distribution can be obtained
querygrain(net1,nodes=c("lung","bronc"), type="joint")

# Evidence can be entered
net12 <- setEvidence(net1, nslist=list(asia="yes", dysp="yes"))
#net12 <- setEvidence(net1,nodes=c("asia", "dysp"), states=c("yes", "yes"))   ##This is other way of entering evidence

# Probability of observing evidence in the model
pEvidence( net12 )

#Network can be queried again
querygrain( net12, nodes=c("lung","bronc") )

querygrain( net12, nodes=c("lung","bronc"), type="joint" )
