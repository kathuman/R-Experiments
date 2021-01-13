# This exercise is based on the page 
# http://www.sharpsightlabs.com/data-analysis-machine-learning-example-part1/

data(Boston, package = "MASS")

View(Boston)

############################
# VISUALIZE TARGET VARIABLE
############################

require(ggplot2)

#~~~~~~~~~~~
# histogram
#~~~~~~~~~~~

ggplot(data = Boston, aes(x = medv)) + geom_histogram()

#~~~~~~~~~~~~~~
# density plot
#~~~~~~~~~~~~~~

ggplot(data = Boston, aes(x = medv)) + stat_density()

head(Boston)

require(reshape2)
melt.boston <- melt(Boston)
head(melt.boston)

ggplot(data = melt.boston, aes(x = value)) +
  stat_density() +
  facet_wrap(~variable, scales = "free")

#~~~~~~~~~~~~~~~~~~~~
# calculate skewness
#~~~~~~~~~~~~~~~~~~~~

install.packages("e1071")
require(e1071)
sapply(Boston, skewness)
