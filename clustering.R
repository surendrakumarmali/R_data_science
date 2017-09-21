##################################################
# k-mean clustering: for numerical values
# Written by Hari
# Last updated: 26072017
#################################################

library(datasets)
library(ggplot2)
View(iris)

###############################################
# cluster with iris data set
##############################################
head(iris)
colnames(iris)
summary(iris)

dim(iris)
str(iris)


# initial scatter plot
ggplot(iris, aes(Petal.Length, Petal.Width)) + geom_point()
ggplot(iris, aes(Petal.Length, Petal.Width, color = Species)) + geom_point()
ggplot(iris, aes(Sepal.Length, Sepal.Width, color = Species)) + geom_point()

ggplot(iris, aes(Sepal.Length, Petal.Width, color = Species)) + geom_point()

ggplot(iris, aes(Petal.Length, Sepal.Width, color = Species)) + geom_point()

ggplot(iris, aes(Petal.Width, Sepal.Width, color = Species)) + geom_point()


# set the seed to ensure reproducibility

set.seed(20)
irisCluster <- kmeans(iris[, 3:4], 3, nstart = 20)
irisCluster
 
irisCluster$cluster
irisCluster$centers
irisCluster$totss
irisCluster$withinss
irisCluster$betweenss
iris$Species
iris$cluster <- irisCluster$cluster

# compare the lebel with cluster
table(irisCluster$cluster, iris$Species)

# plot the cluster
irisCluster$cluster <- as.factor(irisCluster$cluster)
ggplot(iris, aes(Petal.Length, Petal.Width, color = irisCluster$cluster)) + geom_point()


# different variable
irisCluster <- kmeans(iris[, 1:2], 3, nstart = 20)
ggplot(iris, aes(Petal.Length, Petal.Width, color = as.factor(irisCluster$cluster))) + geom_point()
table(irisCluster$cluster, iris$Species)

irisCluster <- kmeans(iris[, c(1, 4)], 3, nstart = 20)
ggplot(iris, aes(Petal.Length, Petal.Width, color = as.factor(irisCluster$cluster))) + geom_point()
table(irisCluster$cluster, iris$Species)



#######################################
# clustering with data generated
#######################################

x <- rbind(matrix(rnorm(100, sd = 0.3), ncol = 2),
matrix(rnorm(100, mean = 1, sd = 0.3), ncol = 2))

colnames(x) <- c("x", "y")

c1 <- kmeans(x, 2)
plot(x)
plot(x, col = c1$cluster)
points(c1$centers, col = 1:2, pch = 8, cex = 2)

# sum of squares
ss <- function(x) sum(scale(x, scale = FALSE)^2)

c1$centers
# cluster centers fitted to each obs
fitted.x <- fitted(c1)
head(fitted.x)
resid.x <- x - fitted(c1)


cbind(c1[c("betweenss", "tot.withinss", "totss")],
c(ss(fitted.x), ss(resid.x), ss(x)))


## random starts do help here with too many clusteres
c2 <- kmeans(x, 5, nstart = 25)
plot(x, col =  c2$cluster)
points(c2$centers, col = 1:5, pch = 8)


###################################################
# library(datasets)
###################################################

str(attitude)
attach(attitude)
View(attitude)
summary(attitude)

dat <- attitude[,c(3,4)]
colnames(dat)
ggplot(dat, aes(privileges, learning)) + geom_point()

set.seed(7)
km1 <- kmeans(dat, 2, nstart = 100)

ggplot(dat, aes(privileges, learning, col = as.factor(km1$cluster))) + geom_point()

mydata <- dat
apply(mydata, 2, var)


####################################################
# ELbow method for selecting optimal no. of cluster
####################################################

wss <- (nrow(mydata)-1) * sum(apply(mydata,2,var))

i = 1
for (i in 2:15) wss[i] <- sum(kmeans(mydata, centers=i)$withinss)

plot(1:15, wss)

plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares",
     main="Assessing the Optimal Number of Clusters with the Elbow Method",
     pch=20, cex=2)


set.seed(7)
km2 = kmeans(dat, 6, nstart=100)
km2

plot(dat, col =(km2$cluster +1) , main="K-Means result with 6 clusters", pch=20, cex=2)

################################################
# k-mode clustering: for categorial values
# Written by Hari
# Last updated on 26072017
###############################################

library(klaR)

### a 5-dimensional toy-example:

## generate data set with two groups of data:
set.seed(1)
x <- rbind(matrix(rbinom(250, 2, 0.25), ncol = 5),
           matrix(rbinom(250, 2, 0.75), ncol = 5))
colnames(x) <- c("a", "b", "c", "d", "e")

## run algorithm on x:
cl <- kmodes(data = x, modes = 2)

cl
## and visualize with some jitter:
plot(jitter(x), col = cl$cluster)
points(cl$modes, col = 1:5, pch = 8)


#############################################
# kmode on iris data set
############################################

set.seed(2)
newiris <- iris
newiris$Species <- NULL
kc <- kmodes(newiris, 3)
table(iris$Species, kc$cluster)

colnames(newiris)
plot(newiris[c("Sepal.Length", "Sepal.Width")], col = kc$cluster)
points(kc$modes[,c("Sepal.Length", "Sepal.Width")], col = 1:3, pch = 8)

############################
# with 3rd and 4th variables
############################
set.seed(8)
kc_mean <- kmeans(newiris[,c(3, 4)], 3)
table(iris$Species, kc_mean$cluster)

colnames(newiris)
plot(newiris[c(3,4)], col = kc_mean$cluster)
points(kc_mean$centers[,c(1,2)], col = 1:3, pch = 8)
