# set.seed(1) # if using R 3.5 or earlier
set.seed(1, sample.kind = "Rounding") # if using R 3.6 or later
disease <- sample(c(0,1), size=1e6, replace=TRUE, prob=c(0.98,0.02))
test <- rep(NA, 1e6)
test[disease==0] <- sample(c(0,1), size=sum(disease==0), replace=TRUE, prob=c(0.90,0.10))
test[disease==1] <- sample(c(0,1), size=sum(disease==1), replace=TRUE, prob=c(0.15, 0.85))

#What is the probability that a test is positive?
prob_pos = sum(test==TRUE)/length(test)

#What is the probability that an individual has the disease if the test is negative?
sum(test[disease==1]==FALSE)/(sum(test[disease==0]==FALSE)+sum(test[disease==1]==FALSE))

#What is the probability that you have the disease if the test is positive?
sum(test[disease==1]==TRUE)/(sum(test[disease==0]==TRUE)+sum(test[disease==1]==TRUE))

#Compare the prevalence of disease in people who test positive to the overall prevalence of disease.
sum(test[disease==1]==TRUE)/(sum(test[disease==0]==TRUE)+sum(test[disease==1]==TRUE))/((sum(disease))/length(disease))


#We are now going to write code to compute conditional probabilities for being male in the heights dataset.

library(dslabs)
data("heights")
heights %>% 
  mutate(height = round(height)) %>%
  group_by(height) %>%
  summarize(p = mean(sex == "Male")) %>%
qplot(height, p, data =.)

#In the plot we just made in Q6 we see high variability for low values of height. This is because we have few data points. This time use the quantile  0.1,0.2,…,0.9  and the cut() function to assure each group has the same number of points. Note that for any numeric vector x, you can create groups based on quantiles like this: cut(x, quantile(x, seq(0, 1, 0.1)), include.lowest = TRUE).

ps <- seq(0, 1, 0.1)
heights %>% mutate(g = cut(height, quantile(height, ps), include.lowest = TRUE)) %>%
  group_by(g) %>%
  summarize(p = mean(sex == "Male"), height = mean(height)) %>%
  qplot(height, p, data =.)

#You can generate data from a bivariate normal distrubution using the MASS package using the following code:
Sigma <- 9*matrix(c(1,0.5,0.5,1), 2, 2)
dat <- MASS::mvrnorm(n = 10000, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))

ps <- seq(0, 1, 0.1)
dat %>% mutate(g = cut(x, quantile(x, ps), include.lowest = TRUE)) %>%
  group_by(g) %>%
  summarize(y = mean(y), x = mean(x)) %>% qplot(x, y, data =.)


