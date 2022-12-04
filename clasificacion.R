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
