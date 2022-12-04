rm(list=ls())

library(ISLR2)       #set de datos
library(ggplot2)     #crear graficos
#library(lattice)    #visualizacion de datos
library(caret)       #para separar datos
library(MASS)        #para las funciones de clasificacion
library(class)       #para KNN
library(e1071)       #para SVM y NB
library(pROC)        #para las curvas ROC



########################### ANALISAR EL SET DE DATOS ###########################

data(Smarket)
summary(Smarket)
head(Smarket)
names(Smarket) # puede haber data y etiquetas
#nci.labs <- NCI60$labs
#nci.data <- NCI60$data
dim(Smarket)
table(Smarket) # es para hacer un especie de groupby()

#scaled_data = scale(Caravan) #se escalan los datos si es necesario
#head(Caravan)

cor(Smarket[, -9]) #matriz de correlaciones entre predictores. Todas las variables tienen que ser cuantitativas
# una correlacion de 0 significa que dichas variables tienen poca correlacion.

############################## SEPARAR LOS  DATOS ##############################

set.seed(123)       #para que los datos que genera sean reproducibles
indexes <- createDataPartition(Smarket$Direction,p=0.8,list=F)
train <- Smarket[indexes,]
test <- Smarket[-indexes,]
dim(test)

#################################   LOGISTICA    ###############################

glm.fit <- glm(Direction~Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, family=binomial, data=train) #ajusto modelo
summary(glm.fit)                                        #se puede mirar el signo de los coef beta. Ademas interesan los p chicos.
summary(glm.fit)$coef                                   #solo los coeficientes
glm.probs <- predict(glm.fit, test, type="response")    #prediccion

contrasts(Smarket$Direction)   #muestra la asignacion de etiquetas

glm.pred <-rep ("Down",249)             #acordarse de corregir de acuerdo al tamano del vector y a la etiqueta
glm.pred[glm.probs >.5] <- "Up"         #acordarse de corregir de acuerdo a la etiqueta

table(glm.pred ,test$Direction)        #matriz de confusion
mean(glm.pred == test$Direction)       #accuracy
mean(glm.pred != test$Direction)       #error rate

glm_roc = roc(test$Direction ~ glm.probs, plot = TRUE, print.auc = TRUE ) #para la curva ROC

"Si quiero ajustar solo con algunos predictores:
glm.fit <- glm(Purchase ~ MRELGE + PPERSAUT + PLEVEN + PBRAND, family=binomial, data=train) 

Si quiero predecir con valores especificos de los predictores:
glm.probs <- (glm.fits, newdata = data.frame(variable1 = c(x1 , x2) , Lag2 = c(x1' , x2')), type = 'response')"

########################  LINEAR DISCRIMINAN ANALYSIS (LDA) ####################

lda.fit <- lda(Direction~., data=train)          #ajusto modelo, revisar si se usan todas o algunas variables
lda.fit                                          #los coef proporcionan la comb lineal de los predictores para la regla de decisión LDA. 
plot(lda.fit)                                    #plotear las distribuciones de los grupos


lda.pred <- predict(lda.fit, test[,-9])         #sacar la columna que tiene los valores a predecir
names(lda.pred)                                 #me da los outputs de lda.pred
table(lda.pred$class,test$Direction)
mean(lda.pred$class == test$Direction)
mean(lda.pred$class != test$Direction)

lda_roc = roc(test$Direction ~ lda.pred$posterior[,2], plot = TRUE, print.auc = TRUE ) #ROC curve

"Si quiero usar un umbral de probabilidad posterior diferente al 50% para hacer predicciones:
(ejemplo con un umbral al 90%)
sum(lda.pred$posterior[, 2] > .9)"


######################## QUADRATIC DISCRIMINAN ANALYSIS (QDA) ##################

qda.fit <- qda(Direction~., data = train)    #ajustar modelo
qda.fit
qda.pred <- predict(qda.fit, test[,-9])                       #predecir 

table(qda.pred$class,test$Direction)
mean(qda.pred$class == test$Direction)
mean(qda.pred$class != test$Direction)

qda_roc = roc(test$Direction ~ qda.pred$posterior[,2], plot = TRUE, print.auc = TRUE ) #curva ROC

###############################  NAIVE BAYES ###################################

"EL clasificador Bayes modela cada característica cuantitativa utilizando una distribución gaussiana. 
Sin embargo, un método de densidad kernel puede también se puede utilizar para estimar las distribuciones."

nb.fit <- naiveBayes(Direction ~. , data = train)  #Ajustar el modelo
nb.fit                 #La salida contiene la media estimada (primer columna) y la desviación estándar (segunda columna) 
                       #para cada variable en cada clase.
"La media y desviacion estandar tambien se puede calcular como:"
# mean(Lag1[train][Direction[train] == "Down"])
# sd(Lag1[train][Direction[train] == "Down"])

nb.class <- predict(nb.fit , test)  #prediccion, obtengo las clases
table(nb.class , test$Direction)
mean(nb.class == test$Direction)
mean(nb.class != test$Direction)

nb.preds <- predict(nb.fit , test, type = "raw") # prediccion, genero estimaciones de las probabilidades de cada observacion
nb.preds[1:5, ]

##############################  K-NEAREST NEIGHBORS (KNN)  #####################

"genero los datos de entrenamiento y testeo sin las etiquetas"
train_knn <- Smarket[indexes,1:8]
test_knn <- Smarket[-indexes,1:8]
"genero los vectores con las etiquetas reales de los datos de train y test"
train_labels <- Smarket[indexes,9] #poner la columna correcta
test_labels <- Smarket[-indexes,9] #poner la columna correcta


#el grafico debajo muestra el error de clasificacion para diferentes valores de k
error <- c()
for (i in 1:8)   #pruebo con k de 1 a 8
{
  knn.fit <- knn(train_knn, test_knn, cl=train_labels, k = i)
  error[i] = 1- mean(knn.fit == test_labels)
}
ggplot(data = data.frame(error), aes(x = 1:8, y = error)) +
  geom_line(color = "Blue")

"se crea un modelo con el k elegido:"
knn.pred <- knn(train_knn, test_knn, cl=train_labels, k = 2)
table(knn.pred, test_labels)
mean(knn.pred == test_labels)
mean(knn.pred != test_labels)

# curva ROC
ctrl <- trainControl(method="repeatedcv", repeats = 3, classProbs=TRUE, summaryFunction = twoClassSummary)
ctrl
knnFit <- train(Direction ~ ., data = train, method = "knn", trControl = ctrl, preProcess = c("center","scale"), tuneLength = 20)
knnFit
knnPredict <- predict(knnFit, newdata = test_knn , type="prob")
knnROC <- roc(test_labels, knnPredict, levels = levels(test_labels))
plot(knnROC, type="S", print.thres= 0.5, print.auc = TRUE)

