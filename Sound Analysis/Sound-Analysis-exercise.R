########### Example making a wave file ourselves########
library(tuneR)
help(tuneR)
help(Wave)

sr <- 8000
t <- seq(0,2,1/sr)
y <- (2^15-1)*sin(2*pi*440*t) #a wave of 440 Hz scaled to fit the 16 bit range
w = Wave(y, samp.rate = sr, bit = 16) # Make the wave representation
play(w)
str(w)

########### Example with existing soundfile#############
install.packages("tuneR", repos = "http://cran.r-project.org")
library(tuneR)

#Functions
sound_dist <- function(duration, samplingrate) {
  #Speed of sound is 1125 ft/sec
  return((duration/samplingrate)*1125/2)
}
sound_data <- function(dataset, threshold, samplingrate) {
  dataset <- snap@left
  threshold = 4000
  samplingrate = 44100
  data <- data.frame()
  max = 0
  maxindex = 0
  for (i in 1:length(dataset)) {
    if (dataset[i] > max) {
      max = dataset[i]
      maxindex = i
      data <- data.frame()
    }
    if (abs(dataset[i]) > threshold) {
      data <- rbind(data, c(i,dataset[i], sound_dist(i - maxindex, samplingrate)))
    }
  }
  colnames(data) <- c("x", "y", "dist")
  return(data)
}

#Analysis
snap <- readWave("Snap.wav")
print(snap) #gives characteristics of the wave object just uploaded
play(snap)
plot(snap@left[46500:47500], type = "l", main = "Snap",xlab = "Time", ylab = "Frequency")
plot(snap@left, type = "l", main = "Snap",xlab = "Time", ylab = "Frequency")
data <- sound_data(snap@left, 4000, 44100)
plot(data[,3], data[,2], type = "l", main = "Snap",xlab = "Dist", ylab = "Frequency")

################ Other Example ####################
library("tuneR") # in a regular session, we are loading tuneR

# constructing a mono Wave object (2 sec.) containing sinus 
# sound with 440Hz and folled by 220Hz:
Wobj <- bind(sine(440), sine(220))
show(Wobj)
plot(Wobj) # it does not make sense to plot the whole stuff
plot(extractWave(Wobj, from = 1, to = 500))
## Not run: 
play(Wobj) # listen to the sound

## End(Not run)

tmpfile <- file.path(tempdir(), "testfile.wav")
# write the Wave object into a Wave file (can be played with any player):
writeWave(Wobj, tmpfile)
# reading it in again:
Wobj2 <- readWave(tmpfile)

Wobjm <- mono(Wobj, "left") # extract the left channel
# and downsample to 11025 samples/sec.:
Wobjm11 <- downsample(Wobjm, 11025)
# extract a part of the signal interactively (click for left/right limits):
## Not run: 
Wobjm11s <- extractWave(Wobjm11)

## End(Not run)
# or extract some values reproducibly 
Wobjm11s <- extractWave(Wobjm11, from=1000, to=17000)

# calculating periodograms of sections each consisting of 1024 observations,
# overlapping by 512 observations:
WspecObject <- periodogram(Wobjm11s, normalize = TRUE, width = 1024, overlap = 512)
# Let's look at the first periodogram:
plot(WspecObject, xlim = c(0, 2000), which = 1)
# or a spectrogram
image(WspecObject, ylim = c(0, 1000))
# calculate the fundamental frequency:
ff <- FF(WspecObject)
print(ff)
# derive note from FF given diapason a'=440
notes <- noteFromFF(ff, 440)
# smooth the notes:
snotes <- smoother(notes)
# outcome should be 0 for diapason "a'" and -12 (12 halftones lower) for "a"
print(snotes) 
# plot melody and energy of the sound:
melodyplot(WspecObject, snotes)

# apply some quantization (into 8 parts): 
qnotes <- quantize(snotes, WspecObject@energy, parts = 8) 
# an plot it, 4 parts a bar (including expected values):
quantplot(qnotes, expected = rep(c(0, -12), each = 4), bars = 2)
# now prepare for LilyPond
qlily <- quantMerge(snotes, 4, 4, 2)
qlily
