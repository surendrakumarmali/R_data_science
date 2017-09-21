#################################################
# written by Hari
# mtcars data analysis
# IDA (Initial data analysis)
# EDA (Exploratory data analysis)
###############################################

rm(list = ls())

#####################################
# library
#######################################

library(ggplot2)

############################
# load mt cars data
########################### 

data(mtcars)
# view the data
View(mtcars)
# dimensions
dim(mtcars)
#name and type of variables
str(mtcars)
#first few records
head(mtcars)
# statistical summary
summary(mtcars)


# create factors with value labels

mtcars$gear = factor(mtcars$gear, levels = c(3, 4, 5), labels = c("3gears", "4gears", "5gears"))
mtcars$am = factor(mtcars$am, levels = c(0, 1), labels = c("Automatic", "Manual"))
mtcars$cyl = factor(mtcars$cyl, levels = c(4, 6, 8), labels = c("4cyl", "6cyl", "8cyl"))

# kernel density plots for mpg
# full data
qplot(mpg, data = mtcars, geom = "density")
# based on gear lablels
qplot(mpg, data = mtcars, geom = "density", fill = gear)
# tranparant on labels
qplot(mpg, data = mtcars, geom = "density", fill = gear, alpha = I(0.5))
p1 <- qplot(mpg, data = mtcars, geom = "density", fill = gear, alpha = I(0.5), main = "Distribution of Gas Milage", xlab = "Miles Per Gallon", ylab = "Density")
p1

# scatterplot of mpg vs hp for each combination of gears and cylinders

# full data
qplot(hp, mpg, data = mtcars)
# full but colored based on labels 
qplot(hp, mpg, data = mtcars, color = am)
# separate window for gear and cyl combination
qplot(hp, mpg, data = mtcars, color = am, facets = gear ~ cyl)
p2 <- qplot(hp, mpg, data = mtcars, shape = am, color = am, facets = gear~cyl, size = I(3), xlab = "Horsepower", ylab = "Miles per Gallon")
p2

# full data
qplot(wt, mpg, data = mtcars)
qplot(wt, mpg, data = mtcars, color = cyl, main = "Regressioin of MPG on Weight", xlab = "Weight", ylab = "Miles per Gallon")

p3 <- qplot(wt, mpg, data = mtcars, color = cyl, main = "Regressioin of MPG on Weight", xlab = "Weight", ylab = "Miles per Gallon")
p3

#Boxplot of mpg by number of gears

p4 <- qplot(gear, mpg, data = mtcars, fill = gear, main = "Mileage by Gear Number",
      xlab = "", ylab = "Miles per Gallon")
p4


#grid display

library(cowplot)
plot_grid(p1, p2, p3, p4, nrow = 2)
