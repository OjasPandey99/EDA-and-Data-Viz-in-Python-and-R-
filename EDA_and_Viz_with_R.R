## Loading the necessary libraries 
library(tidyverse)
library(e1071)
library(ggplot2)
library(lattice)
library(stats)
library(skimr)
library(summarytools)

# Getting the working directory 
getwd()

# You can also change the working directory using setwd() command

# Loading and viewing the csv files
msleep <- read.csv("msleep.csv")
mpg <- read.csv("mpg.csv")

View(msleep)
View(mpg)

dim(msleep)
dim(mpg)

head(msleep)
head(mpg)

# Some EDA techniques

str(msleep)
str(mpg)

glimpse(msleep)

skim(msleep)

summary(msleep)

dfSummary(msleep)

# Counting the missing values in the dataframe 
sum(is.na(msleep))
sum(is.na(mpg))

# Checking the column names 
names(msleep)
names(mpg)

# Dropping the first two columns in msleep data
msleep <- msleep[,-c(1,2)]
dim(msleep)

## Plotting categorical variables 
str(mpg)

# We will work on the "class" feature of mpg dataframe
c1 <-  mpg$class
c1

# Getting the frequency of the feature "class"
c1_freq <- table(c1)
c1_freq

# Plotting the c1_freq variable 

barplot(c1_freq,beside=TRUE)

# Adding legend and color 
barplot(c1_freq, beside=TRUE,
        legend.text = TRUE,
        col=c(2,3))

# Adding a title
barplot(c1_freq, main="Bar plot of class of cars")

# Using qplot
qplot(c1, data=mpg)

# Adding a title and changeing label names
qplot(c1, data=mpg, 
      main="Car class distribution",
      xlab = "Class",
      ylab= "Frequency")

# Plotting a pie chart 
pie(c1_freq)

## Plotting numerical/quantitative variables

# Plots for univariate variables
# Assigning a variable to a column/feature and performing statistical analysis
sleeptime <- msleep$Total.Sleep.Time
summary(sleeptime)
fivenum(sleeptime)

var(sleeptime)
sd(sleeptime)

# Histogram
hist(sleeptime)  # From graphics library
histogram(sleeptime)  # From lattice library

# Setting number of bins 
histogram(sleeptime, breaks=10)
hist(sleeptime, 10)

# Using proportions or probabilities
hist(sleeptime, probability = TRUE)

# Add a colour and a title
hist(sleeptime, probability = TRUE,
     col=gray(0.9),
     main = "Sleeptime")

# Superimpose a normal curve
lines(density(sleeptime), col='red', lw=4)

# Using the qplot function
qplot(sleeptime, data =msleep, main="Hist for mammal sleep time")

# Use the densityplot function
densityplot(sleeptime, lw=4, col='red')

# Using the qplot function
qplot(sleeptime, data =msleep, geom='density',
      main='Mammal sleep time distribution',
      xlab='mammal sleep time',
      ylab= 'Density')

# Density related plots 
qqnorm(sleeptime)
qqline(sleeptime, col=2, lw=3)

# Boxplot
boxplot(sleeptime, main='Title',horizontal=TRUE)
boxplot(msleep$awake, main='Title', horizontal=TRUE)

# Plotting numerical and categorical variable
# Dot plot
qplot(class, hwy, data =mpg, 
      main='Highway miles per gallon',
      xlab='Class of cars',
      ylab='Highway miles per gallon')

# Boxplot
qplot(class, hwy, data =mpg, geom="boxplot",
      main='Highway miles per gallon',
      xlab='Class of cars',
      ylab='Highway miles per gallon')

# Colored density plot 
den <- qplot(class, hwy,fill=class, data =mpg,geom='density',alpha=I(0.8), 
            main='Highway miles per gallon',
            xlab='Class of cars',
            ylab='Highway miles per gallon')
den

## Plotting 2 categorical or 2 numerical variables 
# Check the frequency of two categorical variables
table(mpg$class, mpg$drv)

# Colored bar plot for two categorical variables
qplot(class, fill=drv, data=mpg, geom = 'bar',
      main='Car drive type',
      xlab='Class',
      ylab='Frequency')

# Plotting two numerical variables 
# Scatter plot using the plot function
plot(mpg$cty, mpg$hwy,
     col='darkgreen')

plot(mpg$cty, mpg$hwy,
     col='darkgreen', pch=5,
     main='Title',
     xlab='x',
     ylab='y')

qplot(cty, hwy, data =mpg, 
      main='Title',
      xlab='x',
      ylab='y')

## Plotting three variables 

#Colored Dot plot
qplot(cty, hwy, col=class,data =mpg, 
      main='Title',
      xlab='x',
      ylab='y')

# Save last plot as an image
ggsave("Colored City miles per gallon.jpeg", width=9, height=6)

# Save any plot from global environment as an image
ggsave("Colored density plot.jpeg",den, width=9, height=6)

# Another library I like to use 
library(DataExplorer)
plot_intro(msleep)
plot_missing(msleep)
plot_histogram(msleep)
plot_correlation(na.omit(msleep))
create_report(msleep)
