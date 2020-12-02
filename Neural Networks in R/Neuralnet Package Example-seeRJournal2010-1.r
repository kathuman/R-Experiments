#This is an example obtained from a R Journal article
# found in http://journal.r-project.org/archive/2010-1/RJournal_2010-1_Guenther+Fritsch.pdf

library(neuralnet)
nn <- neuralnet(case~age+parity+induced+spontaneous,data=infert, hidden=2, err.fct="ce",linear.output=FALSE)
nn
nn$result.matrix
nn$covariate 
nn$response
nn$data
nn$net.result
out <- cbind(nn$covariate,nn$net.result[[1]])
dimnames(out) <- list(NULL,c("age","parity","induced","spontaneous","nn-output"))
head(out)
plot(nn)

par(mfrow=c(2,2))
gwplot(nn,selected.covariate="age",min=-2.5, max=5)
gwplot(nn,selected.covariate="parity",min=-2.5, max=5)
gwplot(nn,selected.covariate="induced",min=-2.5, max=5)
gwplot(nn,selected.covariate="spontaneous",min=-2.5, max=5)
