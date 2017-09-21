#Loading libraries for dataset
library(MASS)

#Explore the dataset
?Boston
names(Boston)
head(Boston)
summary(Boston)
str(Boston)

#Some Basic pot
pairs(medv ~ ., data = Boston)
plot(medv ~ lstat, data = Boston)


#Lets Split the dataset into 450(train (350, 100)),56 (test) 
set.seed(123)
h = sample(nrow(Boston),size =  450)
fullTrain = Boston[h,]
test = Boston[-h,]

set.seed(123)
h2 = sample(nrow(fullTrain),size =  350)
train = fullTrain[h2,]
valid = fullTrain[-h2,]

#Checking Baseline Model
mean(fullTrain$medv) 

fit1 = lm(medv ~ lstat, data = train)
summary(fit1)
pred1 = predict(fit1,valid)
SSE1 = sum((pred1-valid$medv)^2)
SST1 = sum((mean(train$medv) - valid$medv)^2)
R2 = 1 - (SSE1/SST1)
RMSE1 = sqrt(SSE1/nrow(valid))

fit2 = lm(medv ~ ., data = train)
summary(fit2)
pred2 = predict(fit2,valid)

#Loading Metrics library for RMSE function
library(Metrics)
RMSE2 = rmse(valid$medv,pred2)

fit3 = lm(medv ~ . - indus - chas -age, data = train)
summary(fit3)
pred3 = predict(fit3,valid)

RMSE3 =rmse(valid$medv,pred3)

############################################

#Create some model from your side
##########################################


#Checking this with Test test
pred11 = predict(fit1,test)
rmse(test$medv,pred11)

pred12 = predict(fit2,test)
rmse(test$medv,pred12)

pred13 = predict(fit3,test)
rmse(test$medv,pred13)

#Checking after training with full train set
fit1 = lm(medv ~ lstat, data = fullTrain)
pred21 = predict(fit1,test)
fit2 = lm(medv ~ ., data = fullTrain)
pred22 = predict(fit2,test)
fit3 = lm(medv ~ . - indus - chas -age, data = fullTrain)
pred23 = predict(fit3,test)

#RMSE with Validation approach and small dataset (For comparison)
rmse(test$medv,pred11)
rmse(test$medv,pred12)
rmse(test$medv,pred13)

#Calculate RMSE with Full train set
rmse(test$medv,pred21)
rmse(test$medv,pred22)
rmse(test$medv,pred23)

#Lets do some feature enginering
fit4 = lm(medv ~ . + I(lstat^2), data = fullTrain)
pred24 = predict(fit4,test)

rmse(test$medv,pred24)

#Just for plotting
fit5 = lm(medv ~ lstat + I(lstat^2), data = fullTrain)
pred25 = predict(fit5,test)

rmse(test$medv,pred25)

#Some plots again
plot(fullTrain$medv~fullTrain$lstat)
points(fullTrain$lstat,fitted(fit1),col="red",pch=20)
points(fullTrain$lstat,fitted(fit5),col="blue",pch=20)


