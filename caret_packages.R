library(rpart)
data("tissue_gene_expression")
set.seed(1991)
x <- tissue_gene_expression$x
y <- tissue_gene_expression$y
train_rpart<-train(x,y,method="rpart", tuneGrid = data.frame(cp=seq(0, 0.1, 0.01)))
plot(train_rpart)
#best cp
train_rpart$bestTune

set.seed(1991)
train_rpart_control <- with(tissue_gene_expression, 
            train(x, y, method = "rpart",
                  tuneGrid = data.frame(cp = seq(0, 0.1, 0.01)), 
                  control = rpart.control(minsplit = 0)))
confusionMatrix(train_rpart_control)

plot(train_rpart_control$finalModel, margin=0.1)
text(train_rpart_control$finalModel)


#We can see that with just seven genes, we are able to predict the tissue type. 
#Now let's see if we can predict the tissue type with even fewer genes using a Random Forest.
#Use the train() function and the rf method to train a Random Forest model and save it to an object called fit. 
#Try out values of mtry ranging from seq(50, 200, 25) (you can also explore other values on your own). What mtry value maximizes accuracy? To permit small nodesize to grow as we did with the 
#classification trees, use the following argument: nodesize = 1.

set.seed(1991)
fit <- with(tissue_gene_expression, 
            train(x, y, method = "rf",
                  tuneGrid = data.frame(mtry = seq(50, 200, 25)), 
                  nodesize = 1))
confusionMatrix(fit)
varImp(fit)

tree_terms <- as.character(unique(fit_rpart$finalModel$frame$var[!(fit_rpart$finalModel$frame$var == "<leaf>")]))
tree_terms