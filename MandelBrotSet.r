## Option1
### Reproduced from http://tolstoy.newcastle.edu.au/R/help/05/10/13198.html
### Written by Jarek Tuszynski, PhD.
rm(list=ls())
# install.packages("fields")
library(fields) # for tim.colors
library(caTools) # for write.gif
m = 1000 # grid size
C = complex( real=rep(seq(-1.8,0.6, length.out=m), each=m ),imag=rep(seq(-1.2,1.2, length.out=m), m ) )
C = matrix(C,m,m)


Z = 0
X = array(0, c(m,m,20))
for (k in 1:20) {
    Z = Z^2+C
    X[,,k] = exp(-abs(Z))
}
image(X[,,k], col=tim.colors(256)) # show final image in
write.gif(X, "Mandelbrot.gif", col=tim.colors(256), delay=100)
#####################################################################

# Option2

########################################################################### 
# Mandelbrot set 
########################################################################### 
mandelbrot <- function(x = c(-3.0, 1.0), # x coordinates 
                       y = c(-1.8, 1.8), # y coordinates 
                       b = .05, # by 'steps' 
                       iter = 20) # maximum number of iterations 
{ 
    m = NULL # will store the results, this is the 'image' matrix 
    
    for(i in seq(x[1], x[2], by = b)) { 
        r = NULL # stores part of the iteration results 
        for(j in seq(y[1], y[2], by = b)) { 
            it = iter # will hold iteration at which point (i, j) breaks off 
            c = c(i, j) # initial point 
            z = c(i, j) # i: real part; j: imaginary part 
            for(k in 1:iter) { 
                # the Mandelbrot iteration formulae: z -> z*z + c 
                z = c(z[1]^2 - z[2]^2, 2 * z[1]*z[2]) + c 
                # tests if point breaks off 
                if((z[1] + z[2])^2 > 4) { it = k; break } 
            } 
            r = c(r, it) # stores iteration results 
        } 
        # constructs the 'image' matrix 
        m = rbind(m, r) 
    } 
    # the output fractal object 
    fractal = list(x = seq(x[1], x[2], by = b), # x coordinates 
                   y = seq(y[1], y[2], by = b), # y coordinates 
                   z = m) # it matrix 
} 

###################################################################### 
# here goes how it works 
###################################################################### 
frac <- mandelbrot() 
image(frac) # perhaps not very nice! 
# more resolution, beware, this might take some time to calculate 
frac <- mandelbrot(b = .004) 
image(frac) 
# now here comes the fun, lets do a persp plot of the set! 
persp(log(frac$z), border = NA, theta = 225, phi = 45, col = "grey", shade= .4) 
###END### 
