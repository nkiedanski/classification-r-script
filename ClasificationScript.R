
##CLASIFICACIÓN MULTICLASE CON SET DE DATOS IRIS

##primero cargamos las bibliotecas para usar el set de datos y las funciones

library(ggplot2)
library(lattice)
library(caret)
library(MASS)

data(iris)
summary(iris)
#Compactly display the internal structure of an R object, a diagnostic function and an alternative to summary
str(iris)
pairs(iris)

##luego dividimos la muestra en datos de train y test (90%-10% por ejemplo)
set.seed(123) #acordarse del set.seed para que los datos que genera sean reproducibles
indexes<-createDataPartition(iris$Species,p=0.9,list=F)
train<-iris[indexes,]
test<-iris[-indexes,]

########################## AJUSTAMOS CON EL MODELO LDA ####################################

fit_lda<-lda(Species~., data=train)
plot(fit_lda)

##predecimos las etiquetas para los datos a testear
pred_lda<-predict(fit_lda, test[,-5])
pred_lda
data.frame(original=test$Species,pred=pred_lda$class)

table(pred_lda$class,test$Species)
mean(pred_lda$class==test$Species)
mean(pred_lda$class!=test$Species)

# PARA LA CURVA ROC
roc_lda<-multiclass.roc(test$Species, pred_lda$posterior, plot=TRUE, print.auc=TRUE)



########################## AJUSTAMOS CON EL MODELO QDA ####################################

fit_qda<-qda(Species~., data=train)

##predecimos las etiquetas para los datos a testear
pred_qda<-predict(fit_qda, test[,-5])
data.frame(original=test$Species, pred=pred_qda$class)

table(pred_qda$class,test$Species)
mean(pred_qda$class==test$Species)
mean(pred_qda$class!=test$Species)

# para la curva ROC
roc_qda<-multiclass.roc(test$Species, pred_qda$posterior, plot=TRUE, print.auc=TRUE)


########################## AJUSTAMOS CON EL MODELO KNN ####################################

library(class)

iris.data<-iris

#estandarizo los datos para knn ya que no hace una suposición en cuanto a su distribución

iris.scale<-scale(iris[,1:4])
set.seed(1234)

#dividimos la muestra en datos de train y test (90%-10% por ejemplo)
indexesKNN<-createDataPartition(iris$Species,p=0.9,list=F)
trainKNN<-iris.scale[indexesKNN,]
testKNN<-iris.scale[-indexesKNN,]

#genero los vectores con las etiquetas reales de los datos de train y test
trainKNN_labels<-iris[indexesKNN,5]
testKNN_labels<-iris[-indexesKNN,5]

#construimos el clasificador knn
iris.knn<- knn(train=trainKNN,test=testKNN,cl=trainKNN_labels,k=3)
#generamos la matriz de confusión para comparar resultados
tab<-table(iris.knn,testKNN_labels)
tab
#vemos que tan preciso es el modelo
accuracy<-function(x){sum(diag(x)/(sum(rowSums(x))))*100}
accuracy(tab)

#The below plot shows the classification error for different values of k
error <- c()
for (i in 1:15)
{
  knn.fit <- knn(train=trainKNN,test=testKNN,cl=trainKNN_labels, k = i)
  error[i] = 1- mean(knn.fit == testKNN_labels)
}
ggplot(data = data.frame(error), aes(x = 1:15, y = error)) +
  geom_line(color = "Blue")

# contruyo un clasificador para un dado k, en este caso k=3
#ctrl <- trainControl(method="repeatedcv",repeats = 3, classProbs=TRUE, summaryFunction = multiClassSummary)
trainKNN
knnFit <- train(iris$Species, trainKNN, method = "knn", 
                trControl = trainControl(method='cv',number=10) )

knnPredict <- predict(knnFit, newdata = testKNN , type="prob")

knnROC <- roc(iris$Species, knnPredict, levels = levels(iris$Species))
plot(knnROC, type="S", print.thres= 0.5, print.auc = TRUE)

# para la curva ROC
roc_knn<-multiclass.roc(testKNN_labels, iris.knn, plot=TRUE, print.auc=TRUE)


########################## AJUSTAMOS CON EL MODELO NAIVE BAYES ###################################

#In this we assign the data from column 1-4 (features) to variable x, and the class to variable y
x = iris[,-5]
y = iris$Species

#Creating the model with cross validation =10
modelNB = train(x,y,'nb',trControl=trainControl(method='cv',number=10))

#Show the summary model, just type "model"
modelNB

#use predict for getting prediction value, and result class:
predict(modelNB$finalModel, x)

#The last one, we need to know how many error classified, so we need to compare the result of 
#prediction with the class/iris species.
table(predict(modelNB$finalModel,x)$class, y)

#COMPARO RESULTADOS CON LA OTRA FORMA
nb.fit <- naiveBayes(Species ~. , data = train)
nb.class <- predict(nb.fit , test[,-5])
table(nb.class , test$Species)



##############################################################################################

########################## SUPPORT VECTOR MACHINE ###################################

library(e1071)   
data(iris)

n <- nrow(iris)  # Number of observations
ntrain <- round(n*0.75)  # 75% for training set
set.seed(314)    # Set seed for reproducible results
tindex <- sample(n, ntrain)   # Create a random index
train_iris <- iris[tindex,]   # Create training set
test_iris <- iris[-tindex,]   # Create test set
svm1 <- svm(Species~., data=train_iris, method="C-classification", kernal="radial", gamma=0.1, cost=10)
summary(svm1)
svm1$SV # se visualizan los support vectors

plot(svm1, train_iris, Petal.Width ~ Petal.Length, slice=list(Sepal.Width=3, Sepal.Length=4))
"In other words, when visualising the effect of predictor variables on the response you can specify 
which other predictor variables are to be hold constant (i.e. at a fixed value).
So in the example, we are visualising the effect of the predictor variables Petal.Length and Petal.Width 
on the response while keeping Sepal.Width and Sepal.Length constant at the specified values."

prediction <- predict(svm1, test_iris)
xtab <- table(test_iris$Species, prediction)
xtab
mean(prediction==test_iris$Species)
mean(prediction!=test_iris$Species)

#######################################################################################

#REG.LOGÍSTICA PARA SET "DEFAULT"
attach(Default)
dim(Default)
summary(Default)
head(Default)

library(caret)
trainIndex<-createDataPartition(Default$default,p=0.8,list=FALSE,times=1)

Default_train<-Default[trainIndex,]
Default_test<-Default[-trainIndex,]

glm.fit<-glm(default~., family=binomial, data=Default_train)
glm.probs<-predict(glm.fit,Default_test,type="response")

contrasts(default)

glm.pred<-rep("No",1999)
glm.pred[glm.probs>.5]<-"Yes"

table(glm.pred,Default_test$default)
mean(glm.pred==Default_test$default)
mean(glm.pred!=Default_test$default)
