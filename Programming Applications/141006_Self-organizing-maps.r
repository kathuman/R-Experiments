install.packages("som")
require(som)
rpkm <- Tx_rpkm[, -c(1:4)]
rpkm.f <- filtering(rpkm, lt=10, ut=30000, mmr=2, mmd=10)
# rpkm.f=log(rpkm.f+0.1) # this doesn't really change much of the result
rpkm.f.n <- normalize(rpkm.f)
foo <- som(rpkm.f.n, xdim=5, ydim=5, topol="rect", neigh="bubble")
png("../results/clustering.SOM.RNAseq.png",width=800, height=800)
plot(foo,yadj=0.15, main="Expression profiles obtained by self-organizing map (SOM) clustering \nof individual mRNA transcript throughout the time-course", xlab="Stage: D13 - D14 - D15 - D16 - D17 - D18")
dev.off()