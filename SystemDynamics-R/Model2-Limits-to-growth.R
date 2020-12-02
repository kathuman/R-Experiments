# Examples devloped from the book "System Dynamics modeling with R
# by Jim Duggan, Springer Verlag

install.packages("deSolve") 
# Package that solves differential equations
# https://journal.r-project.org/archive/2010-2/RJournal_2010-2_Soetaert~et~al.pdf
# https://cran.r-project.org/web/packages/deSolve/deSolve.pdf
require(deSolve)

# Model 2 is a version of the Limits to growth model, from p.49
# Graphic model is on page 53

require(ggplot2)
install.packages("gridExtra")
require(gridExtra)

START <- 0
FINISH <- 100
STEP <- 0.25

simtime <- seq(START, FINISH, by=STEP)
stocks <- c(sStock=100)
auxs <- c(aCapacity=10000,
          aRef.Availability=1,
          aRef.GrowthRate=0.10)

model <- function(time, stocks, auxs){
  with(as.list(c(stocks, auxs)),{
    aAvailability <- 1 - sStock/aCapacity
    aEffect <- aAvailability / aRef.Availability
    aGrowth.Rate <- aRef.GrowthRate*aEffect
    fNet.Flow <- sStock*aGrowth.Rate
    dS_dt <- fNet.Flow
    return(list(c(dS_dt),NetFlow=fNet.Flow,
                GrowthRate=aGrowth.Rate, Effect=aEffect,
                Availability=aAvailability))
  })
}

o <- data.frame(ode(y = stocks, times = simtime, func = model, parms = auxs, method = "euler"))

head(o)
summary(o[,-c(1,5,6)])

require(ggplot2)
ggplot()+
  geom_line(data=o,aes(time,o$sStock),colour="blue")+
  geom_point(data=o,aes(time,o$sStock),colour="blue")+
  scale_y_continuous(labels = waiver())+
  ylab("Stock")+
  xlab("time")

ggplot()+
  geom_line(data=o,aes(time,o$NetFlow),colour="blue")+
  geom_point(data=o,aes(time,o$NetFlow),colour="blue")+
  scale_y_continuous(labels = waiver())+
  ylab("NetFlow")+
  xlab("time")

ggplot()+
  geom_line(data=o,aes(time,o$Availability),colour="blue")+
  geom_point(data=o,aes(time,o$Availability),colour="blue")+
  scale_y_continuous(labels = waiver())+
  ylab("Availability")+
  xlab("time")

ggplot()+
  geom_line(data=o,aes(time,o$GrowthRate),colour="blue")+
  geom_point(data=o,aes(time,o$GrowthRate),colour="blue")+
  scale_y_continuous(labels = waiver())+
  ylab("GrowthRate")+
  xlab("time")

