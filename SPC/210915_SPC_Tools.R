install.packages("qcc")

require(qcc)
data(pistonrings)
attach(pistonrings)

diameter <- qcc.groups(diameter, sample)

# draw a X-Chart
obj <- qcc(diameter[1:25,], type="xbar")

summary(obj)

# Plot new and calibrated data
obj <- qcc(diameter[1:25,], type="xbar",newdata=diameter[26:40,])

# Plot only the new data
plot(obj, chart.all=FALSE)

