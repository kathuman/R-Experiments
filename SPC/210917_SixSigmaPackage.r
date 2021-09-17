# https://www.sixsigmawithr.com/

#install.packages("SixSigma")
require(SixSigma)

hist(ss.data.pc$pc.volume,
     main = "Printer Cartridge Volume",
     xlab = "Volume",
     col = "#BBBBBB",
     border = "white",
     bg = "red",
     freq = FALSE,
     ylim = c(0,0.4))

curve(dnorm(x,16,1),
      add = TRUE,
      lty = 2,
      lwd = 2)

lines(density(ss.data.pc$pc.volume),
      lwd = 2)

text(label = expression(paste(mu==16,
                              "; ",
                              sigma==1,
                              sep = "")),
     x = 16.5,
     y = 0.4,
     adj = c(0,1))

grid()

box()

# Run Chart (R-Chart)

plot(ss.data.pc$pc.volume,
     type = "b",
     pch = 16,
     ylim = c(12,20),
     axes = FALSE,
     main = "Run Chart for the Volume",
     sub = "Printer Cartridge Example",
     xlab = "Run",
     ylab = "Volume")

axis(1,
     at = 1:24,
     cex.axis = 0.7)

axis(2)
box()
grid()
abline(h = 16,
       lwd = 2)