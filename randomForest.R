library(rpart)
n <- 1000
sigma <- 0.25
set.seed(1) # if using R 3.5 or ealier
x <- rnorm(n, 0, 1)
y <- 0.75 * x + rnorm(n, 0, sigma)
dat <- data.frame(x = x, y = y)
fit <- rpart(y ~ ., data = dat)
plot(fit,margin=0.1)
text(fit,cex=0.75)

dat %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(x, y)) + geom_step(aes(x, y_hat), col=2)

library(randomForest)
fit <- randomForest(y ~ x, data = dat)
  dat %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(x, y)) +
  geom_step(aes(x, y_hat), col = "red")

#error
plot(fit)

#with node size of 50 and a maximum of 25 nodes
fit <-  randomForest(y ~ x, data = dat, nodesize = 50, maxnodes = 25)
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(x, y)) +
  geom_step(aes(x, y_hat), col = "red")