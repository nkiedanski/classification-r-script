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

data(Caravan)
summary(Caravan)
head(Caravan)
names(Caravan) # puede haber data y etiquetas
#nci.labs <- NCI60$labs
#nci.data <- NCI60$data
dim(Caravan)
table(Caravan) # es para hacer un especie de groupby()

#scaled_data = scale(Caravan) #se escalan los datos si es necesario
#head(Caravan)

cor(Caravan[, -86]) #matriz de correlaciones entre predictores. Todas las variables tienen que ser cuantitativas
# una correlacion de 0 significa que dichas variables tienen poca correlacion.

############################## SEPARAR LOS  DATOS ##############################

set.seed(123)       #para que los datos que genera sean reproducibles
indexes <- createDataPartition(Caravan$Purchase,p=0.8,list=F)
train <- Caravan[indexes,]
test <- Caravan[-indexes,]
dim(test)

#################################   LOGISTICA    ###############################

glm.fit <- glm(Purchase~., family=binomial, data=train) #ajusto modelo
summary(glm.fit)                                        #se puede mirar el signo de los coef beta. Ademas interesan los p chicos.
summary(glm.fit)$coef                                   #solo los coeficientes
glm.probs <- predict(glm.fit, test, type="response")    #prediccion

contrasts(Purchase)   #muestra la asignacion de etiquetas

glm.pred <-rep ("No",1163)
glm.pred[glm.probs >.5] <- "Yes"

table(glm.pred ,test$Purchase)        #matriz de confusion
mean(glm.pred == test$Purchase)       #accuracy
mean(glm.pred != test$Purchase)       #error rate

glm_roc = roc(test$Purchase ~ glm.probs, plot = TRUE, print.auc = TRUE ) #para la curva ROC

"Si quiero ajustar solo con algunos predictores:
glm.fit <- glm(Purchase ~ MRELGE + PPERSAUT + PLEVEN + PBRAND, family=binomial, data=train) 

Si quiero predecir con valores especificos de los predictores:
glm.probs <- (glm.fits, newdata = data.frame(variable1 = c(x1 , x2) , Lag2 = c(x1' , x2')), type = 'response')"

########################  LINEAR DISCRIMINAN ANALYSIS (LDA) ####################

lda.fit <- lda(Purchase~., data=train)           #ajusto modelo
lda.fit                                          #los coef proporcionan la comb lineal de los predictores para la regla de decisión LDA. 
plot(lda.fit)                                    #plotear las distribuciones de los grupos


lda.pred <- predict(lda.fit, test[,-86])         #sacar la columna que tiene los valores a predecir
names(lda.pred)                                  #me da los outputs de lda.pred
table(lda.pred$class,test$Purchase)
mean(lda.pred$class == test$Purchase)
mean(lda.pred$class != test$Purchase)

lda_roc = roc(test$Purchase ~ lda.pred$posterior[,2], plot = TRUE, print.auc = TRUE ) #ROC curve

"Si quiero usar un umbral de probabilidad posterior diferente al 50% para hacer predicciones:
(ejemplo con un umbral al 90%)
sum(lda.pred$posterior[, 2] > .9)"


######################## QUADRATIC DISCRIMINAN ANALYSIS (QDA) ##################

qda.fit <- qda(Purchase~., data = train)    #ajustar modelo
qda.fit
qda.pred <- predict(qda.fit, test[,-86])                       #predecir 

table(qda.pred$class,test$Purchase)
mean(qda.pred$class == test$Purchase)
mean(qda.pred$class != test$Purchase)

lda_roc = roc(test$Purchase ~ qda.pred$posterior[,2], plot = TRUE, print.auc = TRUE ) #curva ROC



