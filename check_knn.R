library(caret)
library(dslabs)
library(purrr)

y <- heights$height
set.seed(1)
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
train_set <- heights %>% slice(-test_index)
test_set <- heights %>% slice(test_index)

ks <- seq(1, 101, 3)
F_m <- sapply(ks, function(k){
  fit <- knn3(sex ~ height, data = train_set, k = k)
  y_hat <- predict(fit, test_set, type = "class") %>% factor(levels = levels(train_set$sex))
  F_meas(data = y_hat, reference = test_set$sex)
})

max(F_m)
ks[which.max(F_m)]


library(dslabs)
library(caret)
data("tissue_gene_expression")
set.seed(1, sample.kind="Rounding")
x<-tissue_gene_expression$x
y<-tissue_gene_expression$y 
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
train_set_x <- x[-test_index ,]
train_set_y <- y[-test_index]
test_set_x <- x[test_index, ]
test_set_y <- y[test_index ]

ks = seq(1, 11, 2)
Accuracy <- sapply(ks, function(k){
  fit <- knn3(train_set_x, train_set_y , k = k)
  y_hat <- predict(fit, test_set_x, type = "class")%>% factor(levels = levels(train_set_y ))
  cm_test <- confusionMatrix(data = y_hat, reference = test_set_y)
  c(k,cm_test$overall["Accuracy"])
})
Accuracy