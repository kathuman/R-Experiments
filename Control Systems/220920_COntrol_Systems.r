library(control)
m <- 1;  k <- 1;  b <- 0.2
TF("1/(m*s^2 + b *s + k)")

J <- 3.2284E-6;
b <- 3.5077E-6;
K <- 0.0274;
R <- 4;
L <- 2.75E-6;
P_motor <- TF("K/(s*((J*s + b)*(L*s + R) + K^2))")
time <- seq(0,0.2,0.001)
stepplot(P_motor, t = time)

pole(P_motor)

P_motor2 <- feedback(P_motor, 1)
print(P_motor2)

stepplot(P_motor2, t = time)

damp(P_motor2)

bode(tf(100, c(1,6,100)))
bode(ssdata(tf(100, c(1,6,100))))

## MIMO plot
A1 <- rbind(c(0,1), c(-25,-4)); B1 <- rbind(c(1,1), c(0,1))
C1 <- rbind(c(1,0), c(0,1)); D1 <- rbind(c(0,0), c(0,0))
sys1 <- ss(A1,B1,C1,D1)
bodeplot(sys1)

res <- initial(tf(1, c(1,2,1)))
res$y
res$t
A <- rbind(c(-2, -1), c(1,0)); B <- rbind(1,0);
C <- cbind(0,1); D <- as.matrix(0);
x0 <- matrix(c( 0.51297, 0.98127))
initialplot(ss(A,B,C,D), x0)
initialplot(tf(1, c(1,2,1)), t = seq(0, 10, 0.1))
## Not run: State-space MIMO systems
A <- rbind(c(0,1), c(-25,-4)); B <- rbind(c(1,1), c(0,1));
C <- rbind(c(1,0), c(0,1)); D <- rbind(c(0,0), c(0,0))
res <- initial(ss(A,B,C,D))
res$y # has two rows, i.e. for two outputs
initialplot(ss(A,B,C,D))

signal <- gensig('square',4,10,0.1)
H <- tf(c(2, 5, 1),c(1, 2, 3))
response <- lsim(H, signal$u, signal$t)
plot(signal$t, response$y, type = "l", main = "Linear Simulation Response", col = "blue")
lines(signal$t, signal$u, type = "l", col = "grey")
grid(5,5, col = "lightgray")
## Not run: based on example at: https://www.mathworks.com/help/ident/ref/lsim.html
## Not run: MIMO system response
A <- rbind(c(0,1), c(-25,-4)); B <- rbind(c(1,1), c(0,1))
C <- rbind(c(1,0), c(0,1)); D <- rbind(c(0,0), c(0,0))
response <- lsim(ss(A,B,C,D), cbind(signal$u, signal$u), signal$t)
plot(signal$t, response$y[1,], type = "l",
     main = "Linear Simulation Response", col = "blue"); grid(7,7)
plot(signal$t, response$y[2,], type = "l",
     main = "Linear Simulation Response", col = "blue"); grid(7,7)

signal <- gensig('square',4,10,0.1)
H <- tf(c(2, 5, 1),c(1, 2, 3))
lsimplot(H, signal$u, signal$t)
## Not run: MIMO system response
A <- rbind(c(0,1), c(-25,-4)); B <- rbind(c(1,1), c(0,1))
C <- rbind(c(1,0), c(0,1)); D <- rbind(c(0,0), c(0,0))
lsimplot(ss(A,B,C,D), cbind(signal$u, signal$u), signal$t)

nyquist(tf(100, c(1,6,100)))

nyquist(ssdata(tf(100, c(1,6,100))))

## Not run: MIMO plot
A1 <- rbind(c(0,1), c(-25,-4)); B1 <- rbind(c(1,1), c(0,1))
C1 <- rbind(c(1,0), c(0,1)); D1 <- rbind(c(0,0), c(0,0))
sys1 <- ss(A1,B1,C1,D1)
nyquistplot(sys1)
           