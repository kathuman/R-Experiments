install.packages("cubature")
install.packages("yuima", repos="http://R-Forge.R-project.org")
require("yuima")
require(cubature)
m1 <- setModel(drift="0", diffusion="1", state.var="x", time.var="t", solve.var="x", xinit=100)
X<- simulate(m1)
plot(X)
d1 <- X@data@original.data
d1

#Let us simulate 1000 times
grid <- setSampling(Terminal=1, n=1000)
X <- simulate(m1,sampling=grid)
plot(X)
