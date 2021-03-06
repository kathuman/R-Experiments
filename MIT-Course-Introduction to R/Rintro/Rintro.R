## 			  ━━━━━━━━━━━━━━━━━━━
## 			   INTRODUCTION TO R


## 			       Ista Zahn
## 			  ━━━━━━━━━━━━━━━━━━━

##             The Institute For Quantitative Social Science




## Table of Contents
## ─────────────────

## 1 Workshop Materials and Introduction
## 2 Graphical User Interfaces
## 3 Data and Functions
## 4 Getting data into R
## 5 Data Manipulation
## 6 Basic Statistics and Graphs
## 7 Wrap-up


## 1 Workshop Materials and Introduction
## ═════════════════════════════════════


## 1.1 Materials and setup
## ───────────────────────

##   Everyone should have R installed on their laptop–if not:

##   • Open a web browser and go to [http://cran.r-project.org] and
rm(list=ls())
setwd("~/R/MIT-Course-Introduction to R/Rintro")
##     download and install it
##   • Also helpful to install RStudo (download from [http://rstudio.com])

##   Materials for this workshop include slides, example data sets, and
##   example code.

##   • Download materials from [http://j.mp/intro-r]
##   • Extract the zip file containing the materials to your desktop

##   Workshop notes are available in .hmtl and .pdf format. Navigate to
##   your desktop and open either Rintro.pdf or Rintro.html.


## 1.2 What is R?
## ──────────────

##   R is a programming language designed for statistical
##   computing. Notable characteristics include:

##   • Vast capabilities, wide range of statistical and graphical
##     techniques

##   • Very popular in academia, growing popularity in business:
##     [http://r4stats.com/articles/popularity/]

##   • Written primarily by statisticians

##   • FREE (no cost, open source)

##   • Excellent community support: mailing list, blogs, tutorials

##   • Easy to extend by writing new functions


## 1.3 Coming to R
## ───────────────

##   Comming from…
##   matlab: [http://www.math.umaine.edu/~hiebeler/comp/matlabR.pdf]
##   SciPy: [http://mathesaurus.sourceforge.net/matlab-python-xref.pdf]
##   SAS/SPSS: [http://www.et.bs.ehu.es/~etptupaf/pub/R/RforSAS&SPSSusers.pdf]
##   Stata: [http://www.dss.princeton.edu/training/RStata.pdf]


## 2 Graphical User Interfaces
## ═══════════════════════════

## 2.1 R GUI alternatives (no GUI)
## ───────────────────────────────

##   The old-school way is to run R directly in a terminal

##   [file:images/Rconsole.png]

##   But hardly anybody does it that way anymore!


## 2.2 R GUI alternatives (Windows default)
## ────────────────────────────────────────

##   [file:images/Rgui.png]

##   The default windows GUI is not very good
##   • No parentheses matching or syntax highlighting
##   • No work-space browser


## 2.3 R GUI Alternatives (Rstudio on Mac)
## ───────────────────────────────────────

##   [file:images/Rstudio.png]

##   Rstudio has many useful features, including parentheses matching and
##   auto-completion


## 2.4 R GUI Alternatives (Emacs with ESS)
## ───────────────────────────────────────

##   [file:images/emacs.png]

##   Emacs + ESS is a very powerful combination, but can be difficult to
##   set up


## 2.5 Components of R GUIs
## ────────────────────────

##   • The R console
##     • Displays command history and results
##     • Commands can be typed directly in the console
##     • R Console work disappears once session is closed

##   • A text editor
##     • A plain text editor for writing R code
##     • Good ones will have syntax highlighting, parentheses matching etc.
##     • Anything that modifies your data should be done in a text editor

##   • Graphics windows
##     • View, re-size, and save graphics
##     • A good GUI will allow you to cycle through graph history

##   • Work-space viewer
##     • Some GUIs have work-space browsers that allow you to see stored
##       objects
##     • Very helpful if you are absentminded like me and frequently forget
##       what names you gave your data!


## 2.6 Asking R for help
## ─────────────────────

##   • Start html help, search/browse using web browser
##     • at the R console:

help.start()

##     • or use the help menu from you GUI

##   • Look up the documentation for a function

help(topicName)
help(sd)
?topicName
?sd

##   • Look up documentation for a package

help(package="packageName")
help(package="stats")

##   • Search documentation from R (not always the best way… google often
##     works better)

help.search("topicName")
help.search("Average")

## 2.7 R packages and libraries
## ────────────────────────────

##   There are thousands of R packages that extend R's capabilities.

##   • To view available packages:

library()

##   • To see what packages are loaded:

search()

##   • To load a package:

library("packageName")
library("experiment")

##   • Install new package:

install.packages("packageName")


## 2.8 Things to keep in mind
## ──────────────────────────

##   • Case sensitive, like Stata (unlike SAS)

##   • Comments can be put almost anywhere, starting with a hash mark
##     ('`#''); everything to the end of the line is a comment

##   • The command prompt "`>'" indicates that R is ready to receive
##     commands

##   • If a command is not complete at the end of a line, R will give a
##     different prompt, '`+'' by default

##   • Parentheses must always match (first thing to check if you get an
##     error)

##   • R Does not care about spaces between commands or arguments

##   • Names should start with a letter and should not contain spaces

##   • Can use "." in object names (e.g., "my.data")

##   • Use forward slash ("/") instead of backslash in path names, even on
##     Windows


## 2.9 Exercise 0
## ──────────────

##   1. Try to get R to add 2 plus 2
2+2
##   2. See if you can find the help page for the "mean" topic
help(mean)
##   3. Using any means available, try to figure out how to run a linear
##      regression model in R
#CRAN Multivariate statistics
##   4. Go to [http://cran.r-project.org/web/views/] and skim the topic
##      closest to your field/interests


## 3 Data and Functions
## ════════════════════

## 3.1 Assignment
## ──────────────

##   Values can be assigned names and used in subsequent operations
##   • The `<-' operator (less than followed by a dash) is used to save
##     values
##   • The name on the left gets the value on the right.


x <- 11 # Assign the value 10 to a variable named x
x + 1 # Add 1 to x
y <- x + 1 # Assign y the value x + 1
y


##   Saved variables can be listed, overwritten and deleted

ls() # List variables in workspace
x # Print the value of x
print(x)
x <- 100 # Overwrite x. Note that no warning is given!
x
rm(x) # Delete x
ls()

rm(list=ls()) #removes everything

x1<-1
x2<-2
y<-3

rm(list=ls(patter="x")) #removes everything with an x

## 3.2 Functions
## ─────────────

##   Using R is mostly about applying *functions* to *variables*. Functions
##   • take *variable(s)* as input *argument(s)*
##   • perform operations
##   • *return* values which can be *assigned*
##   • optionally perform side-effects such as writing a file to disk or
##     opening a graphics window

##   The general form for calling R functions is

FunctionName(arg.1, arg.2, ... arg.n)
args(ls) #to see the arguments the function requires

##   Arguments can be matched by position or name
# whitespaces do not matter
##   Examples:

#?sqrt
a <- sqrt(y) # Call the sqrt function with argument x=y
args(round)
round(a, digits = 2) # Call round() with arguments x=x and digits=2

# Functions can be nested so an alternative is
round(sqrt(y), digits = 5) # Take sqrt of a and round


## 4 Getting data into R
## ═════════════════════

## 4.1 The gss dataset
## ───────────────────

##   The next few examples use a subset of the General Social Survey data
##   set. The variables in this subset include

head(read.csv("dataSets/gssInfo.csv")) 
#see gssInfo.csv for rest of the variable descriptions


## 4.2 The "working directory" and listing files
## ─────────────────────────────────────────────

##   R knows the directory it was started in, and refers to this as the
##   "working directory". Since our workshop examples are in the Rintro
##   folder on the desktop, we should all take a moment to set that as our
##   working directory:

setwd("C:/Users/kathuman/Documents/R/MIT-Course-Introduction to R/Rintro")


##   We can also set the working directory using paths relative to the
##   current working directory:


getwd() # get the current working directory

setwd("dataSets") # set wd to the dataSets folder
getwd()
setwd("..") # set wd to enclosing folder ("up")


##   It can be convenient to list files in a directory without leaving R

list.files("dataSets") # list files in the dataSets folder
# list.files("dataSets", pattern = ".csv") # restrict to .csv files


## 4.3 Importing data from files
## ─────────────────────────────

##   In order to read data from a file, you have to know what kind of file
##   it is. The table below lists the functions needed to import data from
##   common file formats.

##   ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
##    data type                function                 package         
##   ───────────────────────────────────────────────────────────────────
##    comma separated (.csv)   read.csv()               utils (default) 
##    other delimited formats  read.table()             utils (default) 
##    Stata (.dta)             read.dta()               foreign         
##    SPSS (.sav)              read.spss()              foreign         
##    SAS (.sas7bdat)          read.sas7bdat()          sas7bdat        
##    Excel (.xls, .xlsx)      readWorksheetFromFile()  XLConnect       
##   ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

args(read.csv)
library()
search()
install.packages("XLConnect")
library("XLConnect")
help(package="XLConnect")
##   Examples:

# read gss data from the gss.rds R file
datGSS <- readRDS("dataSets/gss.rds")
# read gss data from the gss.csv comma separated file
gss.data <- read.csv("dataSets/gss.csv") # read gss data
# read a Stata dataset from gss.dta 
library(foreign) # load foreign data functions
datGSS <- read.dta(file="dataSets/gss.dta")


## 4.4 Checking imported data
## ──────────────────────────

##   Always a good idea to examine the imported data set–usually we want
##   the results to be a `data.frame'

class(datGSS) # check to see that test is what we expect it to be
dim(datGSS) # how many rows and columns?
names(datGSS)[1:10] # first 10 column names - equivalent to colnames(). there is also a rownames()
str(datGSS[1:5]) # more details about the first 5 columns - structure

methods(class=class(datGSS)) #question asked on what methods exist for the specific class

## 4.5 Saving and loading R  workspaces
## ────────────────────────────────────

##   In addition to importing individual datasets, R can save and load
##   entire workspaces
##   • Save our entire workspace

ls() # list objects in our workspace
save.image(file="myWorkspace.RData") # save workspace 
rm(list=ls()) # remove all objects from our workspace 
ls() # list stored objects to make sure they are deleted


##   • Load the "myWorkspace.RData" file and check that it is restored


load("myWorkspace.RData") # load myWorkspace.RData
ls() # list objects


##   When you close R you will be asked if you want to save your workspace
##   – if you choose yes then your workspace will be restored next time you
##   start R


## 4.6 Exercise 2: loading and manipulating data
## ─────────────────────────────────────────────

##   1. Load the foreign package if you haven't already done so
##      (`library(foreign)')
search()
##   2. Look at the help page for the read.spss function
help(read.spss)

##   3. Read the SPSS data set in dataSets/gss.sav and assign the result to
##      an R data object named GSS.sav
GSS.sav <- read.spss("dataSets/gss.sav", to.data.frame=TRUE)

##   4. Make sure that the data loaded in step 2 is a data.frame (hint:
##      check the arguments documented in the help page)
class(GSS.sav)

##   5. Display the dimensions of the GSS.sav.
dim(GSS.sav)

##   6. BONUS: figure out how to read the Excel file "gss.xlsx" into R
help(readWorksheetFromFile)
GSSXL<-readWorksheetFromFile("dataSets/gss.xlsx",sheet=1) #this function has 2 required arguments

## 5 Data Manipulation
## ═══════════════════

## 5.1 data.frame objects
## ──────────────────────

##   • Usually data read into R will be stored as a *data.frame*

##   • A data.frame is a list of vectors of equal length
##     • Each vector in the list forms a column
##     • Each column can be a differnt type of vector
##     • Often the columns are variables and the rows are observations

##   • A data.frame has two dimensions corresponding the number of rows and
##     the number of columns (in that order)


## 5.2 data.frame meta-data
## ────────────────────────

##   A number of functions are available for inspecting data.frame objects:


# row and column names
head(names(datGSS)) # variable names in datGSS
head(rownames(datGSS)) # first few rownames of datGSS
# dimensions
dim(datGSS)
# structure
str(datGSS) # get structure


## 5.3 Logical operators
## ─────────────────────

##   It is often useful to select just those rows of your data where some
##   condition holds–for example select only rows where sex is 1
##   (male). The following operators allow you to do this:

##   ==: equal to
##   !=: not equal to
##   >: greater than
##   <: less than
##   >=: greater than or equal to
##   <=: less than or equal to
##   &: and
##   |: or

##   Note the double equals signs for testing equality. The following
##   example show how to use some of these operators to extract and replace
##   elements matching specific conditions.


## 5.4 Extracting subsets of data.frames
## ─────────────────────────────────────

##   You can flexibly extract subsets of data.frames using single brackets
##   • The first index corresponds to rows, the second to columns
##   • Empty index means "all"


# extracting subsets by position
datGSS[c(1,2), ] # rows 1 and 2, all columns, function c combines
datGSS[ , c(1,2)] # all rows, columns 1 and 2

# extracting subsets by name
datGSS[ , c("age", "educ")] # same as above

datGSS[1:5, 1] # rows 1 through 5, column 1
datGSS[1:5, "educ"] # rows 1-5, column "educ"

#logical indexing
datGSS[datGSS[, "age"] > 90, c("sex", "age")] # rows where age > 90
datGSS[datGSS[, "age"] > 80 & datGSS[, "age"] < 90, c("sex", "age")] # rows where age between 80 and 90

datGSS[datGSS[, "age"] > 80 & datGSS[, "age"] < 90 & datGSS$sex=="Male", c("sex", "age")] # rows where age between 80 and 90
datGSS[with(datGSS, age > 80 & age < 90 & sex=="Male"), c("sex", "age")]
datGSS[with(datGSS, age %in% 70:80 & sex=="Male"), c("sex", "age")]
#more complicated sequences, see seq function
?seq
?with
##   Note the use of the `c()' function to combine arguments


## 5.5 Replacing subsets of data.frames
## ────────────────────────────────────

##   You can flexibly create and replace subsets of data.frames using
##   bracket notation


# creating new variable mean centered age
datGSS[ , "ageC"] <- datGSS[, "age"] - mean(datGSS[, "age"])

 #education difference between wifes and husbands
datGSS[ , "educ.diff"] <- datGSS[ , "wifeduc"] - datGSS[, "husbeduc"]

# replacing subsets to create young/old variable
datGSS[ , "young"] <- "no" # all values of young = "no"
datGSS[datGSS[ , "age"] < 30, "young"] <- "yes" # change to "yes" if age < 30

datGSS[1:4, c("age", "ageC", "young", "wifeduc", "husbeduc", "educ.diff")]


## 5.6 Exporting Data
## ──────────────────

##   Now that we have made some changes to our GSS data set, we might want
##   to save those changes to a file. Everything we have done so far has
##   only modified the data in R; the files have remained unchanged.


# write data to a .csv file
write.csv(datGSS, file = "gss.csv")
# write data to a Stata file
write.dta(datGSS, file = "gss.dta")
# write data to an R file
saveRDS(datGSS, file = "gss.rds")


## 5.7 Exercise 3: Data manipulation
## ─────────────────────────────────

##   Use the gss.rds data set

##   1. Generate the following variables:
##      • "rich" equal to 0 if rincdol is less than 100000, and 1 otherwise
##      • "sinc" equal to incomdol - rincdol
datGSS2 <- readRDS("dataSets/gss.rds")
str(datGSS2)
datGSS2[,"rich"] <- 0
datGSS2[datGSS2[,"rincdol"]>=100000,"rich"] <- 1

str(datGSS2)
datGSS2[,"sinc"] <- datGSS2[,"incomdol"]-datGSS2["rincdol"]

##   2. Create a subset of the data containing only rows where "usecomp" =
##      "Yes"

YesUSecomp <- datGSS[datGSS[,"useweb"]=="Yes",]
NoUsecomp <- datGSS[datGSS[,"useweb"]=="No",]
str(YesUSecomp$satjob)

##   3. Examine the data.frame created in step 2, and answer the following
##      questions:
##      • How many rows does it have?
##      • How many columns does it have?
##      • Is the "satjob" variable numeric?

##BONUS##
####      • "dual.earn" equal to 1 if wkftwife = 1 and wkfthusb = 1, and zero
##        otherwise


## 6 Basic Statistics and Graphs
## ═════════════════════════════

## 6.1 Basic statistics
## ────────────────────

##   Descriptive statistics of single variables are straightforward:

mean(datGSS[ , "educ"]) # calculate mean of x
sd(datGSS[, "educ"]) # calculate standard deviation of x
summary(datGSS[ , "educ"]) # calculate min, max, quantiles, mean
summary(datGSS)
summary(datGSS[,1:5])

##   If you get tired of typing the data.frame name over and over, use
##   `with()' instead

with(datGSS,
     c(Lowest = min(educ),
       Average = mean(educ),
       Highest = max(educ))
     )


##   Some of these functions (e.g., summary) will also work with
##   data.frames and other types of objects


## 6.2 Counts and proportions
## ──────────────────────────

##   Start by using the `table()' function to tabulate counts, then perform
##   additional computations if needed

sex.counts <- table(datGSS[, "sex"]) # tabulate sex categories
sex.counts
prop.table(sex.counts) # convert to proportions


##   Add variables for crosstabs


table(datGSS[, c("sex", "happy")]) # crosstab marital X happy


## 6.3 Statistics by classification factors
## ────────────────────────────────────────

##   The `by()' function can be used to perform a calculation separately
##   for each level of a classifying variable

by(datGSS[, c("income", "educ")],
   INDICES=datGSS["sex"],
   FUN=summary)


## 6.4 Correlations
## ────────────────

##   Let's look at correlations among between age, income, and education

cor(datGSS[ , c("age", "incomdol", "educ")])


##   For significance tests, use cor.test()

with(datGSS,
     cor.test(age, educ))


## 6.5 Multiple regression
## ───────────────────────

##   Modeling functions generally use the /formula/ interface whith DV on
##   left followed by "~" followed by predictors–for details see

help("formula")


##   • Predict the number of hours individuals spend on email (emailhrs)

m1 <- lm(educ ~ sex + age, data = datGSS)
summary(m1)


## 6.6 Save R output to a file
## ───────────────────────────

##   Earlier we learned how to write a data set to a file. But what if we
##   want to write something that isn't in a nice rectangular format, like
##   the results of our regression model? For that we can use the `sink()'
##   function:


sink(file="output.txt", split=TRUE) # start logging
print("This is the result from model 1\n")
print(summary(m1))
sink() ## sink with no arguments turns logging off


## 6.7 Basic graphics: Frequency bars
## ──────────────────────────────────

##   Thanks to classes and methods, you can `plot()' many kinds of objects:


plot(datGSS[ , "marital"]) # Plot a factor

##   [file:images/examplePlot1.png]


## 6.8 Basic graphics: Boxplots by group
## ─────────────────────────────────────

##   Thanks to classes and methods, you can `plot()' many kinds of objects:

with(datGSS,
     plot(marital, educ)) # Plot ordinal by numeric

##   [file:images/examplePlot2.png]


## 6.9 Basic graphics: Mosaic chart
## ────────────────────────────────

##   Thanks to classes and methods, you can `plot()' many kinds of objects:

with(datGSS, # Plot factor X factor
     plot(marital, happy))

##   [file:images/examplePlot3.png]


## 6.10 Exercise 3
## ───────────────

##   Using the datGSS data.frame

##   1. Cross-tabulate sex and emailhrs
##   2. Calculate the mean and standard deviation of incomdol by sex
##   3. Save the results of the previous two calculations to a file
##   4. Create a scatter plot with educ on the x-axis and incomdol on the
##      y-axis


## 7 Wrap-up
## ═════════

## 7.1 Help us make this workshop better!
## ──────────────────────────────────────

##   • Please take a moment to fill out a very short feedback form

##   • These workshops exist for you – tell us what you need!

##   • [http://tinyurl.com/R-intro-feedback]


## 7.2 Additional resources
## ────────────────────────

##   • IQSS workshops:
##     [http://projects.iq.harvard.edu/rtc/filter_by/workshops]

##   • IQSS statistical consulting: [http://rtc.iq.harvard.edu]

##   • Software (all free!):
##     • R and R package download: [http://cran.r-project.org]
##     • Rstudio download: [http://rstudio.org]
##     • ESS (emacs R package): [http://ess.r-project.org/]

##   • Online tutorials
##     • [http://www.codeschool.com/courses/try-r]
##     • [http://www.datamind.org]

##   • Getting help:
##     • Documentation and tutorials:
##       [http://cran.r-project.org/other-docs.html]
##     • Recommended R packages by topic:
##       [http://cran.r-project.org/web/views/]
##     • Mailing list: [https://stat.ethz.ch/mailman/listinfo/r-help]
##     • StackOverflow: [http://stackoverflow.com/questions/tagged/r]
