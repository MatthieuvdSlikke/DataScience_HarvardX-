options(digits=7)
# set.seed(1986) # if using R 3.5 or earlier
set.seed(1986, sample.kind="Rounding") # if using R 3.6 or later
n <- round(2^rnorm(1000, 8, 1))
# set.seed(1) # if using R 3.5 or earlier
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
mu <- round(80 + 2*rt(1000, 5))
range(mu)
schools <- data.frame(id = paste("PS",1:1000),
                      size = n,
                      quality = mu,
                      rank = rank(-mu))
schools %>% top_n(10, quality) %>% arrange(desc(quality))

# set.seed(1) # if using R 3.5 or earlier
set.seed(1) # if using R 3.6 or later
mu <- round(80 + 2*rt(1000, 5))

scores <- sapply(1:nrow(schools), function(i){
  scores <- rnorm(schools$size[i], schools$quality[i], 30)
  scores
})
schools <- schools %>% mutate(score = sapply(scores, mean))
schools %>% top_n(10, score) %>% arrange(desc(score))

#Compare the median school size to the median school size of the top 10 schools based on the score.
#What is the median school size overall?
median(schools$size)
#What is the median school size of the of the top 10 schools based on the score?
schools %>% top_n(10, score)%>% summarize(median(size))
#What is the median school size of the bottom 10 schools based on the score?
schools %>% top_n(-10, score)%>% summarize(median(size))

#Plot the average score versus school size to see what's going on. Highlight the top 10 schools based on the true quality.
schools %>% ggplot(aes(size, score)) + geom_point(alpha = 0.5) + geom_point(data = filter(schools, rank<=10),col = 3) 

#Let's use regularization to pick the best schools. Remember regularization shrinks deviations from the average towards 0. 
#To apply regularization here, we first need to define the overall average for all schools, using the following code:
overall <- mean(sapply(scores, mean))

lambda <- 25
schools_avgs <- schools %>% 
  mutate( reg = overall + (score - overall) * size / (size + lambda))

schools_avgs %>% top_n(10, reg) %>% arrange(desc(reg))

#Notice that this improves things a bit. The number of small schools that are not highly ranked is now lower. 
#Is there a better  𝛼 ? Using values of  𝛼  from 10 to 250, find the  𝛼  that minimizes the RMSE
#What value of  𝛼  gives the minimum RMSE?

lambdas <- seq(10,250)
rmses <- sapply(lambdas, function(l){
  schools_avgs <- schools %>% 
    mutate( reg = overall + (score - overall) * size / (size + l)) %>%
    summarize(rmse = sqrt(1/1000 * sum((reg-quality)^2))) %>% .$rmse
})

lambdas[which.min(rmses)]
plot(lambdas,rmses)

