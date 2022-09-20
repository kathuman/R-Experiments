# https://cran.r-project.org/web/packages/control/README.html
devtools::install_github("benubah/control")

# https://rviews.rstudio.com/2017/08/24/control-systems-toolbox/
library(control)
m <- 1;  k <- 1;  b <- 0.2
TF("1/(m*s^2 + b *s + k)")

# Electric Motor
J <- 3.2284E-6;
b <- 3.5077E-6;
K <- 0.0274;
R <- 4;
L <- 2.75E-6;
P_motor <- TF("K/(s*((J*s + b)*(L*s + R) + K^2))")
time <- seq(0,0.2,0.001)
stepplot(P_motor, t = time)

pole(P_motor)

# Closed Loop
P_motor2 <- feedback(P_motor, 1)
print(P_motor2)

stepplot(P_motor2, t = time)

damp(P_motor2)

sys1 <- ss(1,2,3,4)
sys2 <- ss(2,3,4,5)
sys3 <- ss(6,7,8,9)
sys4 <- tf(1, c(1,2,5))
append(sys1, sys2, sys3)
append(sys1, sys2, sys4, sys3)
