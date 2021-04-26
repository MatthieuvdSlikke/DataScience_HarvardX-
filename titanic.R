library(titanic)    # loads titanic_train data frame
library(caret)
library(tidyverse)
library(rpart)
library(dplyr)
library(purrr)
# 3 significant digits
options(digits = 3)

# clean the data - `titanic_train` is loaded with the titanic package
titanic_clean <- titanic_train %>%
  mutate(Survived = factor(Survived),
         Embarked = factor(Embarked),
         Age = ifelse(is.na(Age), median(Age, na.rm = TRUE), Age), # NA age to median age
         FamilySize = SibSp + Parch + 1) %>%    # count family members
  select(Survived,  Sex, Pclass, Age, Fare, SibSp, Parch, FamilySize, Embarked)


#Split titanic_clean into test and training sets - after running the setup code, 
#it should have 891 rows and 9 variables.
#Set the seed to 42, then use the caret package to create a 20% data 
#partition based on the Survived column. Assign the 20% partition to 
#test_set and the remaining 80% partition to train_set.

#How many observations are in the training set?
set.seed(42)
y <- titanic_clean$Survived
test_index <- createDataPartition(y, times = 1, p = 0.2, list = FALSE)
train_set <- titanic_clean %>% slice(-test_index)
test_set <- titanic_clean %>% slice(test_index)

p<-  mean(train_set$Survived == 1)
p

#The simplest prediction method is randomly guessing the outcome without using 
#additional predictors. These methods will help us determine whether our machine 
#learning algorithm performs better than chance. How accurate are two methods of 
#guessing Titanic passenger survival?
#Set the seed to 3. For each individual in the test set, 
#randomly guess whether that person survived or not by sampling 
#from the vector c(0,1) (Note: use the default argument setting 
#of prob from the sample function).
#What is the accuracy of this guessing method?
set.seed(3)
y_hat <- sample(c(0,1),nrow(test_set), replace=TRUE)
mean(y_hat==test_set$Survived)
#Use the training set to determine whether members of a given sex were more likely 
#to survive or die. Apply this insight to generate survival predictions on the test set.
#What proportion of training set females survived?

female_survived <- train_set %>% filter(Survived ==1)%>%filter(Sex =="female")  %>% nrow()
female <- train_set %>% filter(Sex =="female")  %>% nrow()
female_survived/female

#Predict survival using sex on the test set: if the survival rate for a sex is over 0.5, predict survival for all individuals of that sex, and predict death if the survival rate for a sex is under 0.5.
#What is the accuracy of this sex-based prediction method on the test set?
sex_model <- ifelse(test_set$Sex == "female", 1, 0)
mean(sex_model == test_set$Survived)

#In the training set, which class(es) (Pclass) were passengers more likely to survive than die?
train_set %>% group_by(Pclass) %>% summarize(Survived = mean(Survived == 1))

#Predict survival using passenger class on the test set: predict survival 
#if the survival rate for a class is over 0.5, otherwise predict death.
#What is the accuracy of this class-based prediction method on the test set?
pclass_model <- ifelse(test_set$Pclass == 1, 1, 0)
mean(pclass_model == test_set$Survived)

#Use the training set to group passengers by both sex and passenger class.
#Which sex and class combinations were more likely to survive than die?
train_set %>% group_by(Pclass,Sex) %>% summarize(Survived = mean(Survived == 1)) %>% filter

#Predict survival using both sex and passenger class on the test set. 
#Predict survival if the survival rate for a sex/class combination is over 0.5, 
#otherwise predict death.
#What is the accuracy of this sex- and class-based prediction method on the test set?
sex_pclass_model <- ifelse(test_set$Sex == "female"& test_set$Pclass != 3 , 1, 0)
mean(sex_pclass_model == test_set$Survived)

#Use the confusionMatrix() function to create confusion matrices for the sex model,
#class model, and combined sex and class model. You will need to convert predictions 
#and survival status to factors to use this function.

#What is the "positive" class used to calculate confusion matrix metrics?
confusionMatrix(data = factor(sex_model), reference = factor(test_set$Survived))
confusionMatrix(data = factor(sex_pclass_model), reference = factor(test_set$Survived))
confusionMatrix(data = factor(pclass_model), reference = factor(test_set$Survived))

#Use the F_meas() function to calculate  𝐹1  scores for the sex model, class model,
#and combined sex and class model. You will need to convert predictions to 
#factors to use this function.
#Which model has the highest  𝐹1  score?
F_meas(data = factor(sex_model), reference = factor(test_set$Survived))
F_meas(data = factor(sex_pclass_model), reference = factor(test_set$Survived))
F_meas(data = factor(pclass_model), reference = factor(test_set$Survived))

#Set the seed to 1. Train a model using linear discriminant analysis (LDA) 
#with the caret lda method using fare as the only predictor.
#What is the accuracy on the test set for the LDA model?

set.seed(1)

train_lda <- train(Survived ~ Fare, method="lda", data=train_set)
y_hat_lda <- predict(train_lda,test_set)
mean(y_hat_lda==test_set$Survived)


train_qda <- train(Survived ~ Fare, method="qda", data=train_set)
y_hat_qda <- predict(train_qda,test_set)
mean(y_hat_qda==test_set$Survived)


#Set the seed to 1. Train a logistic regression model with the caret glm method 
#using age as the only predictor.
#What is the accuracy of your model (using age as the only predictor) on the test set ?
train_glm <- train(Survived ~ Age, method="glm", data=train_set)
y_hat_glm <- predict(train_glm,test_set)
mean(y_hat_glm==test_set$Survived)

train_glm_4 <- train(Survived ~ Age + Sex + Pclass + Fare, method="glm", data=train_set)
y_hat_glm_4 <- predict(train_glm_4,test_set)
mean(y_hat_glm_4==test_set$Survived)



train_glm_all <- train(Survived ~., method="glm", data=train_set)
y_hat_glm_all <- predict(train_glm_all,test_set)
mean(y_hat_glm_all==test_set$Survived)


#Set the seed to 6. Train a kNN model on the training set using the caret train 
#function. Try tuning with k = seq(3, 51, 2).
#What is the optimal value of the number of neighbors k?
set.seed(6)

train_knn <- train(Survived ~ ., data = train_set, method = "knn", tuneGrid = data.frame(k=seq(3,51,2)))
train_knn$bestTune

#What is the accuracy of the kNN model on the test set?

y_hat_knn <- predict(train_knn,test_set)
confusionMatrix(y_hat_knn,test_set$Survived)


#Set the seed to 8 and train a new kNN model. Instead of the default training control,
#use 10-fold cross-validation where each partition consists of 10% of the total. 
#Try tuning with k = seq(3, 51, 2).
set.seed(8)

control <- trainControl(method = "cv", number=10, p=.9)

train_knn_cv <- train(Survived ~ ., data = train_set, method = "knn", tuneGrid = data.frame(k=seq(3,51,2)), trControl= control)
train_knn_cv$bestTune
y_hat_knn_cv <- predict(train_knn_cv,test_set)
confusionMatrix(y_hat_knn_cv,test_set$Survived)

#Set the seed to 10. Use caret to train a decision tree with the rpart method. 
#Tune the complexity parameter with cp = seq(0, 0.05, 0.002).
#What is the optimal value of the complexity parameter (cp)?
set.seed(10)
train_rpart <- train(Survived ~ ., data = train_set, method = "rpart", tuneGrid = data.frame(cp=seq(0, 0.05, 0.002)))
train_rpart$bestTune
y_hat_rpart <- predict(train_rpart,test_set)
mean(y_hat_rpart==test_set$Survived)

#Inspect the final model and plot the decision tree.
plot(train_rpart $finalModel, margin=0.1)
text(train_rpart $finalModel)


#Set the seed to 14. Use the caret train() function with the rf method to train a 
#random forest. Test values of mtry = seq(1:7). Set ntree to 100.
#What mtry value maximizes accuracy?
set.seed(14)
train_rf <- train(Survived ~ ., data = train_set, method = "rf", tuneGrid = data.frame(mtry=seq(1:7)),ntree = 100)
train_rf$bestTune
y_hat_rf <- predict(train_rf,test_set)
mean(y_hat_rf==test_set$Survived)
varImp(train_rf)

