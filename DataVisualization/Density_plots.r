library(plotly)
mydata = read.csv("density_plot.txt")
df = as.data.frame(mydata)
plot_ly(df, x = Y, y = X, z = Z, group = X, type = "scatter3d", mode = "lines") 
