options(digits = 3)
library(matrixStats)
library(tidyverse)
library(caret)
library(dslabs)
data(brca)

#The brca dataset from the dslabs package contains information about breast cancer diagnosis biopsy samples for tumors that were determined to be either benign (not cancer) and malignant (cancer). The brca object is a list consisting of:
#brca$y: a vector of sample classifications ("B" = benign or "M" = malignant)
#brca$x: a matrix of numeric features describing properties of the shape and size of cell nuclei extracted from biopsy microscope images
#For these exercises, load the data by setting your options and loading the libraries and data as shown in the code here:

#How many samples are in the dataset?
length(brca$y)

#How many predictors are in the matrix?
ncol(brca$x)

#What proportion of the samples are malignant?
sum(brca$y=="M")/length(brca$y)

#Which column number has the highest mean?
which.max(colMeans(brca$x))

#Which column number has the lowest standard deviation?
which.min(colSds(brca$x))


#Use sweep() two times to scale each column: subtract the column means of brca$x, 
#then divide by the column standard deviations of brca$x.
x_mean <- sweep(brca$x,2,colMeans(brca$x))
x_stand <- sweep(x_mean,2,colSds(x_mean),FUN="/")
#After scaling, what is the standard deviation of the first column?
colSds(x_stand)
#After scaling, what is the median value of the first column?
colMedians(x_stand)


#Calculate the distance between all samples using the scaled matrix.
#What is the average distance between the first sample, which is benign, and other benign samples?
d <- dist(x_stand)
d_B <- as.matrix(d)[1, brca$y == "B"]
mean(d_B[2:length(d_B)])

d_M <- as.matrix(d)[1, brca$y == "M"]
mean(d_M)


#Make a heatmap of the relationship between features using the scaled matrix.
#Which of these heatmaps is correct?
d_features <- dist(t(x_stand))
heatmap(as.matrix(d_features), labRow = NA, labCol = NA)

#Perform hierarchical clustering on the 30 features. Cut the tree into 5 groups.
#All but one of the answer options are in the same group.
#Which is in a different group?
h <- hclust(d_features)
groups <- cutree(h, k = 5)
groups
split(names(groups), groups)

#Perform a principal component analysis of the scaled matrix.
#What proportion of variance is explained by the first principal component?
pca<-prcomp(x_stand)
summary(pca)


#Plot the first two principal components with color representing tumor type (benign/malignant).

data.frame(pca$x[,1:2], tumorType=brca$y) %>% 
  ggplot(aes(PC1,PC2, fill = tumorType))+
  geom_point(cex=3, pch=21) +
  coord_fixed(ratio = 1)

#Make a boxplot of the first 10 PCs grouped by tumor type.

data.frame(tumorType = brca$y, pca$x[,1:10]) %>%
  gather(key = "PC", value = "value", -tumorType) %>%
  ggplot(aes(PC, value, fill = tumorType)) +
  geom_boxplot()


set.seed(1)
test_index <- createDataPartition(brca$y, times = 1, p = 0.2, list = FALSE)
test_x <- x_stand[test_index,]
test_y <- brca$y[test_index]
train_x <- x_stand[-test_index,]
train_y <- brca$y[-test_index]

#What proportion of the test set is benign?
mean(test_y=="B")
#What proportion of the training set is benign?
mean(train_y=="B")

#The predict_kmeans() function defined here takes two arguments - a matrix of 
#observations x and a k-means object k - and assigns each row of x to a cluster from k.

predict_kmeans <- function(x, k) {
  centers <- k$centers    # extract cluster centers
  # calculate distance to cluster centers
  distances <- sapply(1:nrow(x), function(i){
    apply(centers, 1, function(y) dist(rbind(x[i,], y)))
  })
  max.col(-t(distances))  # select cluster with min distance to center
}

#Set the seed to 3. Perform k-means clustering on the training set with 2 centers and assign the output to k.
#Then use the predict_kmeans() function to make predictions on the test set.
#What is the overall accuracy?
set.seed(3)
km <- kmeans(test_x,centers=2)
kp <- ifelse(predict_kmeans(test_x, km) == 1, "B", "M")
mean(kp == test_y)
mean(kp == test_y)

#Fit a logistic regression model on the training set with caret::train() using all predictors. 
#Ignore warnings about the algorithm not converging. Make predictions on the test set.
#What is the accuracy of the logistic regression model on the test set?
train_glm <- train(train_x, train_y,method = "glm")
pglm <- predict(train_glm, test_x)
mean(pglm == test_y)

#Train an LDA model and a QDA model on the training set. 
#Make predictions on the test set using each model.
#What is the accuracy of the LDA model on the test set?
train_lda <- train(train_x, train_y,method = "lda")
plda <- predict(train_lda, test_x)
mean(plda == test_y)
#What is the accuracy of the QDA model on the test set?
train_qda <- train(train_x, train_y,method = "qda")
pqda <- predict(train_qda, test_x)
mean(pqda == test_y)

#Set the seed to 5, then fit a loess model on the training set with the caret package. 
#You will need to install the gam package if you have not yet done so. 
#Use the default tuning grid. This may take several minutes; ignore warnings. 
#Generate predictions on the test set.
#What is the accuracy of the loess model on the test set?
set.seed(5)
train_loess <- train(train_x, train_y,method = "gamLoess")
ploess <- predict(train_loess, test_x)
mean(ploess == test_y)

#Set the seed to 7, then train a k-nearest neighbors model on the training set using the caret package. 
#Try odd values of  𝑘  from 3 to 21. Use the final model to generate predictions on the test set.
#What is the final value of  𝑘  used in the model?
set.seed(7)
train_loess <- train(train_x, train_y,method = "gamLoess")
ploess <- predict(train_loess, test_x)
mean(ploess == test_y)

#Set the seed to 7, then train a k-nearest neighbors model on the training set using the caret 
#package. Try odd values of  𝑘  from 3 to 21. Use the final model to generate predictions on the test set.
#What is the final value of  𝑘  used in the model?
set.seed(7)
train_knn <- train(train_x, train_y, method = "knn", tuneGrid = data.frame(k=seq(3,21,2)))
train_knn$bestTune
y_hat_knn <- predict(train_knn,test_x)
mean(y_hat_knn == test_y)


#Set the seed to 9, then train a random forest model on the training set using the caret package. 
#Test mtry values of c(3, 5, 7, 9). Use the argument importance = TRUE so that feature importance can be extracted. 
#Generate predictions on the test set.
#Note: please use c(3, 5, 7, 9) instead of seq(3, 9, 2) in tuneGrid.
#What value of mtry gives the highest accuracy?
set.seed(9)
train_rf <- train(train_x, train_y, method = "rf", tuneGrid = data.frame(mtry=c(3, 5, 7, 9)), importance = TRUE)
train_rf$bestTune
y_hat_rf <- predict(train_knn,test_x)
mean(y_hat_rf== test_y)
varImp(train_rf)
