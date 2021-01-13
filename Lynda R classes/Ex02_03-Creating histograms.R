# R Statistics Essential Training
# Ex02_03
# Creating histograms for quantitative variables

?lynx
data(lynx)

#Make a histogram using the default function
hist(lynx)

#Modify the histogram
h <- hist(lynx, #Save the histogram as an object
          breaks = 11, #suggests 11 bins 
#             breaks =seq(0,7000, by =100), 
#             breaks =c(0,100,300,500,3000,3500,7000),
          freq = FALSE,
          col = "thistle1", #or use col = colors()[626]
          main = "Histogram of Anual Canadian Lynx Trappings\n1821-1934",
          xlab = "Number of Lynx trapped")

#if freq = FALSE, this will draw a normal distrobution
curve(dnorm(x, mean = mean(lynx), sd = sd(lynx)),
      col = "thistle4",
      lwd = 2,
      add = TRUE)

rm(list = ls())