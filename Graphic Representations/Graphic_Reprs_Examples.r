############# Examples of Graphic representations
######################## Example1
dfEpidemic = data.frame(Outbreak = c("Plague of Justinian", "Black Plague"
                                     , "HIV/AIDS", "1918 Flu", "Modern Plague"
                                     , "Asian Flu", "6th Cholera Pandemic"
                                     , "Russian Flu", "Hong Kong Flut"
                                     , "5th Cholera Pandemic", "4th Cholera Pandemic"
                                     , "7th Cholera Pandemic", "Swine Flu"
                                     , "2nd Cholera Pandemic", "First Cholera Pandemic"
                                     , "Great Plague of London", "Typhus Epidemic of 1847"
                                     , "Haiti Cholera Epidemic", "Ebola"
                                     , "Congo Measles Epidemic", "West African Meningitis"
                                     , "SARS")
                        , Count = c(100000000, 50000000, 39000000, 20000000
                                    , 10000000, 2000000, 1500000, 1000000
                                    , 1000000, 981899, 704596, 570000, 284000
                                    , 200000, 110000, 100000, 20000, 6631
                                    , 4877, 4555, 1210, 774)
                        , FirstYear = c(541, 1346, 1960, 1918, 1894, 1957, 1899, 1889
                                        , 1968, 1881, 1863, 1961, 2009, 1829, 1817
                                        , 1665, 1847, 2011, 2014, 2011, 2009, 2002))
dfEpidemic$Outbreak = factor(dfEpidemic$Outbreak
                             , levels=dfEpidemic$Outbreak[order(dfEpidemic$FirstYear
                                                                , decreasing=TRUE)])
install.packages("Rcpp")
library(ggplot2)
library(scales)
plt = ggplot(data = dfEpidemic, aes(x=Outbreak, y=Count)) + geom_bar(stat="identity") + coord_flip()
plt = plt + scale_y_continuous(labels=comma)
plt

###################### Example2
dfEpidemic$LastYear = c(542, 1350, 2014, 1920, 1903, 1958, 1923, 1890, 1969, 1896, 1879
                        , 2014, 2009, 1849, 1823, 1666, 1847, 2014, 2014, 2014, 2010, 2003)
dfEpidemic$Duration = with(dfEpidemic, LastYear - FirstYear + 1)
dfEpidemic$Rate = with(dfEpidemic, Count / Duration)

plt = ggplot(data = dfEpidemic, aes(x=Outbreak, y=Count, fill=Rate)) + geom_bar(stat="identity")
plt = plt + coord_flip() + scale_y_continuous(labels=comma)
plt

##################### Example3
dfEpidemic2 = dfEpidemic[-(1:2), ]
plt = ggplot(data = dfEpidemic2, aes(x=Outbreak, y=Count, fill=Rate)) + geom_bar(stat="identity")
plt = plt + coord_flip() + scale_y_continuous(labels=comma)
plt

#################### Example4
install.packages("gridExtra")
install.packages("extrafont")
library(ggplot2)
library(scales)
library(RColorBrewer)
library(gridExtra)
library(extrafont)
results=data.frame(size=numeric(0), x=numeric(0))
for (i in seq(10, by=10, length.out = 16)){results=rbind(results, data.frame(size=i, x=replicate(5000, {sum(seq(1:i)-sample(seq(1:i), size=i, replace=FALSE)==0)})))}
opts=theme(
    panel.background = element_rect(fill="gray98"),
    panel.border = element_rect(colour="black", fill=NA),
    axis.line = element_line(size = 0.5, colour = "black"),
    axis.ticks = element_line(colour="black"),
    panel.grid.major.y = element_line(colour="gray80"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(colour="gray25", size=15),
    axis.text.x = element_text(colour="gray25", size=15),
    text = element_text(family="Humor Sans", size=15, colour="gray25"),
    legend.key = element_blank(),
    legend.position = "none",
    legend.background = element_blank(),
    plot.title = element_text(size = 18))
sizes=unique(results$size)
for (i in 1:length(sizes))
{
    data=subset(results, size==sizes[i])
    assign(paste("g", i, sep=""),
           ggplot(data, aes(x=as.factor(x), weight=1/nrow(data)))+
               geom_bar(binwidth=.5, fill=sample(brewer.pal(9,"Set1"), 1), alpha=.85, colour="gray50")+
               scale_y_continuous(limits=c(0,.4), expand = c(0, 0), "Probability", labels = percent)+
               scale_x_discrete(limit =as.factor(0:8), expand = c(0, 0), "Number of matches")+
               labs(title = paste("Matching", as.character(sizes[i]), "items ...", sep=" "))+
               opts)
}
grid.arrange(g1, g2, g3, g4, g5, g6, g7, g8, g9, g10, g11, g12, g13, g14, g15, g16, ncol=4)

##################### Example5
## This is an example of using a volcano plot, as well as interacting with Plotly
install.packages("devtools")  # so we can install from github
library("devtools")
install_github("ropensci/plotly")  # plotly is part of ropensci
library(plotly)

py <- plotly(username="r_user_guide", key="mw5isa4yqp")  # open plotly connection

# Generate data
library(reshape2) # for melt
volcano3d <- melt(volcano)
names(volcano3d) <- c("x", "y", "z")

# Basic plot
v <- ggplot(volcano3d, aes(x, y, z = z))
v + geom_tile(aes(fill = z)) + stat_contour()

py$ggplotly()

################ Example 6
rm(list=ls())
install.packages("devtools")  # so we can install from github
library("devtools")
install_github("ropensci/plotly")  # plotly is part of ropensci
library(plotly)

py <- plotly(username="r_user_guide", key="mw5isa4yqp")  # open plotly connection
ggiris <- qplot(Petal.Width, Sepal.Length, data = iris, color = Species)

py$ggplotly(ggiris)  # send to plotly
