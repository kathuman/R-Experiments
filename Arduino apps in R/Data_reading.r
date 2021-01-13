# This is a program to use Arduino with R. The idea is to read the idea 
# is to read the data from an arduino board directly into R. All this in Linux. For this end
# we have first written a small application in Arduino and identified the file where the data is 
# being stored. 
# Data storage file: /dev/ttyACM0

f <- file("/dev/ttyACM0", open="r")
#scan(f)
d <- scan(f, n=10)
View(d)
close(f)

# this is working