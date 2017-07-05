########################################
# Written by Hari
# SVM for multiclass classification
########################################

library(e1071)
data(iris)
attach(iris)

# iris data has 3 class levels
unique(iris$Species)

# feature matrix
x <- subset(iris, select = -Species)
# target classes
y <- Species

# fitting svm for multiclass cloassification
model <- svm(x, y, probability = TRUE)
pred_prob <- predict(model, x, decision.values = TRUE, probability = TRUE)

attr(pred_prob, "probabilities")
