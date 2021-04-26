library(dslabs)
library(caret)
data(mnist_27)
# set.seed(1995) # if R 3.5 or earlier
set.seed(1995, sample.kind="Rounding") # if R 3.6 or later
indexes <- createResample(mnist_27$train$y, 10)

length(which(indexes$Resample01==3))
length(which(indexes$Resample01==4))
length(which(indexes$Resample01==7))

x <- sapply(indexes, function(ind){
  sum(ind == 3)})
sum(x)

set.seed(1)
M <- replicate( 10000, {
  y <- rnorm(100, 0, 1)
  quantile(y, 0.75)
})
mean(M)
sd(M)

y <- rnorm(100, 0, 1)
N = 10
Bootstrap <- replicate( 10000, {
  X<-sample(y, N, replace = TRUE)
  quantile(X, 0.75)
})
mean(Bootstrap)
sd(Bootstrap)

set.seed(1) 
y <- rnorm(100, 0, 1)
set.seed(1) 
indexes <- createResample(y, 10)
q10 <- sapply(indexes, function(ind){
  y_hat <- y[ind]
  quantile(y_hat, 0.75)})
mean(q10)
sd(q10)
