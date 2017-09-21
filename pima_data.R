######################################################
# initial data analysis
# written by Hari
# date 20170811
######################################################

input_file_path <- "C:\\Users\\i325907\\SAP_labs\\sap_related\\IOT\\r_code\\learning\\data\\pima.txt"
# read the data into R
pima <- read.table(input_file_path, header = T)
# take a look

##########################################
# Initial data analysis
##########################################
pima
head(pima) # top 5 row
dim(pima) # number of row and columns
str(pima) # check what each variables in the dataset represent

# check if variables are quantitative or quanlitative
# if quantitative, continous or discrete
# if quanlitative, whether order exists between levels

# check for anything unusla or unexpected
# indication of a data-entry error, inconsisency

summary(pima) # some numerical summaries

# look at maximum and minimum values of each variables
# It is weired that blood presuure equals zero
# check variables glucose, triceps, insulin, bimi

sort(pima$blood)

# sorted values from small to large
# It seems likely that the zero has been used as a missing value code
# R use "NA" as the missing value code
# Set all zero values of the variables to NA

pima$blood[pima$blood == 0] <- NA
pima$glucose[pima$glucose == 0] <- NA
pima$triceps[pima$triceps == 0] <- NA
pima$insulin[pima$insulin == 0] <- NA
pima$bmi[pima$bmi == 0] <- NA

summary(pima)
str(pima)

# The variable test is a qualitiative variable, whose numerical coding is meaningless
# In R, a qualitiative variable should be assigned as a factor sothat R can handle it in a appropirate way

pima$test <- factor(pima$test)
summary(pima$test)
summary(pima)

# check how variable test is coded now
levels(pima$test)
# It is even better to use descriptive labels
levels(pima$test) <- c("negative", "positive")
levels(pima$test)


# summary after missing value and handling qualitative variables etc
summary(pima)

#################################################
# Exploratory data analysis
#################################################

# histogram to show plots the distribution of variables
hist(pima$blood)

# a bell-shaped distribution for the blood pressure centered around 70
# histogram may obscure some features of the data because its constructon requires some inputs specified by user
#  smoothed version of the histogram might be preferred

plot(density(pima$blood, na.rm = TRUE))

# sorted data against its index
plot(sort(pima$blood), pch = ".")

# one advantage of this plot is that we can see all the cases indiviually, which may offer some information about outliers in addition to the 
#distribution of data

# three plot in a window
par(mfrow = c(1, 3)) # first no. of row, second the number of columns
hist(pima$blood)
plot(density(pima$blood, na.rm=TRUE))
plot(sort(pima$blood), pch=".")

# bi-variate plots
par(mfrow = c(1,2))
plot(pedigree~blood, pima) # scatter plot as variable blood is quantitative variables
plot(pedigree~test, pima) # side by side box plot because the variable test is a qualitative variabel
par(mfrow=c(1,1))

plot(blood~ glucose, pima)

# fit a linear model
m1 <- lm(pima$pregnant ~pima$glucose)
summary(m1)

m2 <- lm(pima$pregnant~pima$glucose+pima$blood)
m3 <-lm(pima$pregnant~pima$glucose+pima$blood+pima$triceps+pima$insulin+pima$bmi)
