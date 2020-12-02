# Examples devloped from the book "System Dynamics modeling with R
# by Jim Duggan, Springer Verlag

install.packages("deSolve") 
# Package that solves differential equations
# https://journal.r-project.org/archive/2010-2/RJournal_2010-2_Soetaert~et~al.pdf
# https://cran.r-project.org/web/packages/deSolve/deSolve.pdf
require(deSolve)

# Model 1 is a 1 stock with inflow and outflow, p.41

# First, define the simulation time constants
Start <- 2015
Finish <- 2030
Step <- 0.25
simtime <- seq(Start, Finish, by=Step)

#reviewing the simtime variable
head(simtime)
tail(simtime)

# Second, two vectors are defined, for the stocks and for the auxiliary variables
# using the Hungarian notation
stocks <- c(sCustomers=10000)
auxs <- c(aGrowthFraction=0.08, aDeclineFraction=0.03)

# Third, create a function to implement the model equations
# this function takes in three variables: stocks, auxs and time
model <- function(time, stocks, auxs){
  with(as.list(c(stocks,auxs)),{
    fRecruits <- sCustomers*aGrowthFraction
    fLosses <- sCustomers*aDeclineFraction
    dC_dt <- fRecruits - fLosses
    return (list(c(dC_dt), 
                 Recruits=fRecruits, Losses=fLosses, 
                 GF=aGrowthFraction, DF=aDeclineFraction))
  })
}

# Fourth and finally, the model is solved by using the ode function
o <- data.frame(ode(y = stocks, times = simtime, func = model, parms = auxs, method = "euler"))

# we can now inspect the output o
head(o)
# we can also see the summary for the outputs that actually change i.e., columns 2,3, and 4
summary(o[,-c(1,5,6)])

# we can also visualize these outputs by using the ggplot2 library
require(ggplot2)
ggplot()+
  geom_line(data=o,aes(time,o$sCustomers),colour="blue")+
  geom_point(data=o,aes(time,o$sCustomers),colour="blue")+
  scale_y_continuous(labels = waiver())+
  ylab("Customers")+
  xlab("Year")

# we can also save the plot to a file
ggsave("customers.png")
