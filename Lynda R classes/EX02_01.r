#EX02_01

#Help on Plots
?plot

data(chickwts)

#Quickest method
plot(chickwts$feed)

#one with more control
#data needs preparation

#Create a summary table
feeds <- table(chickwts$feed)
feeds #see what the result was

barplot(feeds)

#order the barplot
barplot(feeds[order(feeds, decreasing=TRUE)])

#customize the plot
par(oma=c(1,1,1,1)) #sets the outside margins: b,l,t,r
par(mar=c(4,5,2,1)) #sets plot margins

barplot(feeds[order(feeds)],
      horiz=TRUE,
      las=1, #las gives orientation of axis labels
      col=c("beige","blanchedalmond","bisque1","bisque2","bisque3","bisque4"),
      border =NA, #no borders on the bars
      main="Frequencies of Different Feeds\ninchickwts Dataset", #\n is a linebreak
      xlab="Number of Chicks")

?par