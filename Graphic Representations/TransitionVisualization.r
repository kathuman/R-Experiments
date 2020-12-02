install.packages("Gmisc")
library(Gmisc)
library(grid)
# Generate some fake data ###############
set.seed(9730)
b4 <- sample(1:3, replace = TRUE, size = 500, prob = c(0.1, 0.4, 0.5))
after <- sample(1:3, replace = TRUE, size = 500, prob = c(0.3, 0.5, 0.2))
b4 <- factor(b4, labels = c("None", "Moderate", "Major"))
after <- factor(after, labels = c("None", "Moderate", "Major"))
# Create the transition matrix
transition_mtrx <- table(b4, after)
# Create a table with the transitions
htmlTable(transition_mtrx, title = "Transitions", ctable = TRUE)
#########################################

# 1. Simple Plot. Thickness of the lines shows the transition values
transitionPlot(transition_mtrx)

# 2.
# Note that when using my arrow type you need to specify exact unit width
# since I wasn't certain how the lwd should be translated.
transitionPlot(transition_mtrx, overlap_add_width = 1.3, type_of_arrow = "simple", min_lwd = unit(2, "mm"), max_lwd = unit(10, "mm"))

# 3.
# Note that when using my arrow type you need to specify exact unit width
# since I wasn't certain how the lwd should be translated.
transitionPlot(transition_mtrx, overlap_add_width = 1.3, type_of_arrow = "gradient", min_lwd = unit(2, "mm"), max_lwd = unit(10, "mm"))

# 4.
library(RColorBrewer)
transitionPlot(transition_mtrx, txt_start_clr = "black", txt_end_clr = "black", 
               fill_start_box = brewer.pal(n = 3, name = "Pastel1"), 
               fill_end_box = brewer.pal(n = 6, name = "Pastel1")[4:6], 
               overlap_add_width = 1.3, type_of_arrow = "gradient", 
               min_lwd = unit(2, "mm"), max_lwd = unit(10, "mm"))

# 5.
transitionPlot(transition_mtrx, box_prop = cbind(c(0.3, 0.7, 0.5), c(0.5, 0.5, 0.4)), txt_start_clr = c("black", "white"), txt_end_clr = c("black", "white"), fill_start_box = brewer.pal(n = 3, name = "Paired")[1:2], fill_end_box = brewer.pal(n = 3, name = "Paired")[1:2], overlap_add_width = 1.3, type_of_arrow = "gradient", min_lwd = unit(2, "mm"), max_lwd = unit(10, "mm"))
