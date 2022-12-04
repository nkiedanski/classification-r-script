library(caret)
library(MASS)
data(iris)

data<-iris

#Split data into train/test sets using createDataPartition()
data_split <- createDataPartition(data$Species, p = 0.8, list = FALSE)
test <- data[-data_split,] # Save 20% of data for test validation here
dataset <- data[data_split,] # 80% of data

#distribución de clases
percentage <- prop.table(table(dataset$Species)) * 100
cbind(freq=table(dataset$Species), percentage=percentage)

#Separate training data (x) from labels (y)
x <- dataset[,1:4] 
y <- dataset[,5]

#boxplots
par(mfrow=c(1,4))
for(i in 1:4) {
  boxplot(x[,i], main=names(iris)[i])
}

#Scatterplot Matrix
featurePlot(x=x, y=y, plot="ellipse")

#Boxplots for each feature
featurePlot(x=x, y=y, plot="box")

#Density plots
scales <- list(x=list(relation="free"), y=list(relation="free"))
featurePlot(x=x, y=y, plot="density", scales=scales)


#Machine Learning Algorithm Evaluation
control <- trainControl(method="cv", number=10)
metric <- "Accuracy"

#LDA
set.seed(7)
fit.lda <- train(Species~., data=dataset, method="lda", metric=metric, trControl=control)

#CART (Classification and Regression Trees)
set.seed(7)
fit.cart <- train(Species~., data=dataset, method="rpart", metric=metric, trControl=control)

#KNN
set.seed(7)
fit.knn <- train(Species~., data=dataset, method="knn", metric=metric, trControl=control)

#SVM
set.seed(7)
fit.svm <- train(Species~., data=dataset, method="svmRadial", metric=metric, trControl=control)

#Random Forest
set.seed(7)
fit.rf <- train(Species~., data=dataset, method="rf", metric=metric, trControl=control)

#Summarize Model Accuracies
results <- resamples(list(lda=fit.lda, cart=fit.cart, knn=fit.knn, svm=fit.svm, rf=fit.rf))
summary(results)

#Best Model Summary
print(fit.lda)

#Evaluate Best Fit Model on Test Data
predictions <- predict(fit.lda, test)
confusionMatrix(predictions, test$Species)

dotplot(results)

