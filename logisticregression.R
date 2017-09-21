#setting your working directory
rm(list = ls())
setwd("C:\\Users\\i325907\\SAP_labs\\sap_related\\sap_work\\workshop_ml_sap")

# Read in data
polling = read.csv("PollingData.csv")

str(polling)
table(polling$Year)
summary(polling)

# Install and load mice package
library(mice)


# Multiple imputation
simple = polling[c("Rasmussen", "SurveyUSA", "PropR", "DiffCount")]
summary(simple)
set.seed(144)
imputed = complete(mice(simple))
summary(imputed)
polling$Rasmussen = imputed$Rasmussen
polling$SurveyUSA = imputed$SurveyUSA
summary(polling)

# Subset data into training set and test set
train = subset(polling, Year == 2004 | Year == 2008)
test = subset(polling, Year == 2012)

# Smart Baseline
table(train$Republican)
sign(20)
sign(-10)
sign(0)
table(sign(train$Rasmussen))
table(train$Republican, sign(train$Rasmussen))

# Multicollinearity
cor(train)
str(train)
cor(train[c("Rasmussen", "SurveyUSA", "PropR", "DiffCount", "Republican")])

# Logistic Regression Model

mod1 = glm(Republican~PropR, data=train, family="binomial")
summary(mod1)

# Training set predictions
pred1 = predict(mod1, type="response")
table(train$Republican, pred1 >= 0.5)

# Two-variable model
mod2 = glm(Republican~SurveyUSA+DiffCount, data=train, family="binomial")
pred2 = predict(mod2, type="response")
table(train$Republican, pred2 >= 0.5)
summary(mod2)

# Smart baseline accuracy
table(test$Republican, sign(test$Rasmussen))

# Test set predictions


testPrediction = predict(mod2, newdata=test, type="response")
table(test$Republican, testPrediction >= 0.5)

# Analyze mistake
subset(test, testPrediction >= 0.5 & Republican == 0)


library(ROCR)
ROCRpred = prediction(testPrediction, test$Republican)
as.numeric(performance(ROCRpred, "auc")@y.values)

ROCRperf = performance(ROCRpred, "tpr", "fpr")
plot(ROCRperf)
plot(ROCRperf, colorize=TRUE)

