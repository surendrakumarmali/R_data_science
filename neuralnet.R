#######################################################
# written by Hari
# neuralnet package for the training of neural network
#######################################################

# training of neural network using backpropagation


library(neuralnet)

AND <- c(rep(0, 7), 1)
OR <- c(0, rep(1, 7))

binary.data <- data.frame(expand.grid(c(0,1), c(0, 1), c(0, 1)), AND, OR)
print(net <- neuralnet(AND + OR ~ Var1 + Var2 + Var3, binary.data, hidden = 0, rep = 10, err.fct = "ce", linear.output = FALSE))


data(infert, package="datasets")
print(net.infert <- neuralnet(case~parity + induced + spontaneous, infert, err.fct = "ce", linear.output = FALSE, likelihood = TRUE))
gwplot(net.infert, selected.covariate = "parity")
gwplot(net.infert, selected.covariate = "spontaneous")
gwplot(net.infert, selected.covariate = "induced")
confidence.interval(net.infert)


Var1 <- runif(50, 0, 100)
sqrt.data <- data.frame(Var1, Sqrt=sqrt(Var1))
print(net.sqrt <- neuralnet(Sqrt ~ Var1, sqrt.data, hidden = 10, threshold = 0.01))
compute(net.sqrt, (1:10) ^ 2)$net.result


Var1 <- rpois(100, 0.5)
Var2 <- rbinom(100, 2, 0.6)
Var3 <- rbinom(100, 1, 0.5)

SUM <- as.integer(abs(Var1 + Var2 + Var3 + (rnorm(100))))
sum.data <- data.frame(Var1+Var2+Var3, SUM)
print(net.sum <- neuralnet(SUM ~ Var1+ Var2+Var3, sum.data, hidden =1, act.fct="tanh"))
prediction(net.sum)
