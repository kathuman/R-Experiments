#EX01_09
# Working with Color

#Barplot
x = c(12,4,21,17,13,9)
barplot(x)

#R specifies color in several ways
?colors
#Web page with pdf of colors in R
#browseURL("http://research.stowers-institute.org/efg/R/Color/Chart/")

#To get a complete list of the colors names
colors()

barplot(x, col="slategray3")
barplot(x, col=colors()[102]) #darkseagreen
barplot(x, col=colors()[602]) #slategray3

# Also RGB Triplets can be used
?rgb
#We can convert color names to rgb with "col2rgb"
?col2rgb
col2rgb("navyblue") #yields (0.0,128)
barplot(x,col=rgb(.54,.0,.0)) #darkred

barplot(x,col=rgb(159,182,205,max=255)) #back to slategray3

#RGB Hexcodes
#Can also use shortcut hexcodes (base 16) which are equivalent to 
#RBG on the 0-255 scale as FF equals 255 in decimal
barplot(x,col="#FFEBCD") #blanchealmond
barplot(x,col="#9FB6CD") #back to slategray3

#Multiple colors
#Can specify several colors (using any coding method) in a vector
barplot(x,col=c("red","blue")) #colors will cycle
barplot(x,col=c("red","blue","green","yellow"))

#USING COLOR PALETTES
#Palettes can be more attractive amd more informative
#Easiest to use
help(package=colorspace) #lots of info on colorspace
?palette

#Built-in Palettes
# rainbow(n,s=1,v=1, start=0, end=max(1,n-1/n),alpha=1)
#heat.colors(n,alpha=1) #yellow through red
#terrain.colors(n, alpha=1) #gray through green
#topo.colors(n, alpha=1) #purple through tan
#cm.colors(n, alpha=1) #Blues and pinks
help(package=colorspace)

palette() #tells us what our current palette is

#Now doing the bar plot using several of these default colors
barplot(x,col=1:6)
barplot(x,col=rainbow(6))
barplot(x,col=heat.colors(6))
barplot(x,col=terrain.colors(6))
barplot(x,col=topo.colors(6))
barplot(x,col=cm.colors(6))

palette("default") #return to the default

rm(list=ls()) #Clean up
