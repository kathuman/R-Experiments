# https://medium.com/@taposhdr/basics-of-audio-file-processing-in-r-81c31a387e8e

install.packages(c("readr","tuneR"))

library(readr)
library(tuneR)

train_audio <- readWave("audio_file.wav")

str(train_audio)

# convert our sound array to floating point values ranging from -1 to 1 
s1 <- train_audio@left
s1 <- s1 / 2^(train_audio@bit -1)

timeArray <- (0:(18593 - 1)) / train_audio@samp.rate

#Plot the wave
plot(timeArray, s1, type='l', col='black', xlab='Time (ms)', ylab='Amplitude')

#tuneR
m2 <- melfcc(train_audio, numcep=9, usecmp=TRUE, modelorder=8,
             spec_out=TRUE, frames_in_rows=FALSE)

############################################################################
# https://www.r-bloggers.com/2020/04/audio-classification-in-r/

install.packages(c("parallel","tidyverse","abind","caret","warbleR"))
