# documentacion: https://www.rdocumentation.org/packages/pROC/versions/1.18.0/topics/roc
# ejemplo: https://daviddalpiaz.github.io/r4sl/logistic-regression.html#roc-curveslibrary(ISLR2)

attach(Smarket)
plot(Volume)


trainIndex<-createDataPartition(Direction,p=0.8,list=FALSE,times=1)
Smarket_train<-Smarket[trainIndex,]
Smarket_test<-Smarket[-trainIndex,]

# ---------------------------------------------------------------------------------------------------------------------

# REGRESION LOGISTICA

glm.fit<-glm(Direction~Lag1+Lag2, data = Smarket_train, family = binomial)

glm.probs<-predict(glm.fit,Smarket_test,type="response")
glm.probs #tengo las probabilidades numericas, se puede colocar directamente en la funcion roc()

glm.pred<-rep("Down",249)
glm.pred[glm.probs>.5]<-"Up"


library(pROC)
glm.probs = predict(glm.fit,Smarket_test,type="response")
glm_roc = roc(Smarket_test$Direction ~ glm.probs, plot = TRUE, print.auc = TRUE )

#test_prob = predict(model_glm, newdata = default_tst, type = 'response')
#test_roc = roc(default_tst$default ~ test_prob, plot = TRUE, print.auc = TRUE)

# ---------------------------------------------------------------------------------------------------------------------

# QUADRATIC DISCRIMINAN ANALYSIS (QDA)

qda.fit <- qda(Direction ~ Lag1 + Lag2 , data = Smarket_train)
qda.predict <- predict(qda.fit , Smarket_test)
qda.predict$class # tengo la classificacion de cada observacion, esto NO puede ir en funcion roc()
qda.predict$posterior # tengo las probabilidades de cada observacion, esto puede ir en funcion roc()
qda_roc = roc(Smarket_test$Direction ~ qda.predict$posterior[,1], plot = TRUE, print.auc = TRUE )

# ---------------------------------------------------------------------------------------------------------------------

# LINEAR DISCRIMINAN ANALYSIS (LDA)
lda.fit <- lda(Direction ~ Lag1 + Lag2 , data = Smarket_train)
lda.predict <- predict(lda.fit , Smarket_test)
lda.predict$posterior[,1]
lda_roc = roc(Smarket_test$Direction ~ lda.predict$posterior[,1], plot = TRUE, print.auc = TRUE )

# ---------------------------------------------------------------------------------------------------------------------
library(ROCR)

# K-NEAREST NEIGHBORS (KNN)  - no puedo hacerlo con la forma de arriba, hay una forma distica
# ejemplo: https://rstudio-pubs-static.s3.amazonaws.com/16444_caf85a306d564eb490eebdbaf0072df2.html



ctrl <- trainControl(method="repeatedcv",repeats = 3,classProbs=TRUE,summaryFunction = twoClassSummary)

knnFit <- train(Direction ~ ., data = Smarket_train, method = "knn", 
                trControl = ctrl, preProcess = c("center","scale"), tuneLength = 20)

knnPredict <- predict(knnFit,newdata = Smarket_test , type="prob")

knnROC <- roc(Smarket_test$Direction, knnPredict[,"Down"], levels = levels(Smarket_test$Direction))

plot(knnROC, type="S", print.thres= 0.5, print.auc = TRUE)

#######################################################################################################################

