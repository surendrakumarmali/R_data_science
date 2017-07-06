#########################################################
# written by Hari
# sqldf for creating in memory data base and sql funtions
#########################################################

library(sqldf)

data(iris)
attach(iris)

data_path <- "C:\\Users\\i325907\\SAP_labs\\sap_related\\IOT\\r_code\\learning\\data\\"
file_name <- "iris.csv"
write.csv(iris,  paste(c(data_path, file_name), collapse = ""), row.names = FALSE, quote = FALSE)

colnames(iris)
summary(iris)
unique(iris$Species)
is.factor(iris$Species)
is.factor(iris$Sepal.Length)
iris2 <- read.csv.sql(paste(c(data_path,file_name), collapse = ""), sql = "select * from file where Species = 'setosa'")


#subset
data(farms, package = "MASS")
attach(farms)

summary(farms)
sqldf("select * from farms where Manag in ('BF', 'HF')")

