library(caret)
library(dslabs)
library(tidyverse)


#Use the training set to build a model with several of the models available from 
#the caret package. We will test out 10 of the most common machine learning models 
#in this exercise:

models <- c("glm", "lda", "naive_bayes", "svmLinear", "knn", "gamLoess", "multinom", "qda", "rf", "adaboost")

#Apply all of these models using train() with all the default parameters. 
#You may need to install some packages. Keep in mind that you will probably 
#get some warnings. Also, it will probably take a while to train all of the models - 
#be patient!
#Run the following code to train the various models:

set.seed(1)
data("mnist_27")
fits <- lapply(models, function(model){ 
  print(model)
  train(y ~ ., method = model, data = mnist_27$train)
}) 

names(fits) <- models
set.seed(1)
predicted <- sapply(fits,function(model){
  predict(model, newdata =mnist_27$test)
})
dim(predicted)

#Now compute accuracy for each model on the test set.
#Report the mean accuracy across all models.

mean(predicted==mnist_27$test$y)

#Next, build an ensemble prediction by majority vote and compute the accuracy of 
#the ensemble. Vote 7 if more than 50% of the models are predicting a 7, and 2 otherwise.
#What is the accuracy of the ensemble?
vote_7 <- rowMeans(predicted== "7")
y_hat <- ifelse(vote_7 > 0.5, "7", "2")
mean(y_hat == mnist_27$test$y)


#In Q3, we computed the accuracy of each method on the test set and noticed that the 
#individual accuracies varied.
#How many of the individual methods do better than the ensemble?
colMeans(predicted==mnist_27$test$y)

#It is tempting to remove the methods that do not perform well and re-do the ensemble. 
#The problem with this approach is that we are using the test data to make a decision. 
#However, we could use the minimum accuracy estimates obtained from cross validation 
#with the training data for each model from fit$results$Accuracy. Obtain these estimates
#and save them in an object. Report the mean of these training set accuracy estimates.
#What is the mean of these training set accuracy estimates?

acc <- sapply(fits,function(fit)
{min(fit$results$Accuracy)})
mean(acc)

#Now let's only consider the methods with a minimum accuracy estimate of greater than 
#or equal to 0.8 when constructing the ensemble. Vote 7 if 50% or more of those models 
#are predicting a 7, and 2 otherwise
#What is the accuracy of the ensemble now?

ind <- acc >= 0.8
votes <- rowMeans(predicted[,ind] == "7")
y_hat <- ifelse(votes>=0.5, 7, 2)
mean(y_hat == mnist_27$test$y)