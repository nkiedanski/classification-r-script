library(ISLR2)

# ANALISIS DEL DATASET
# para ver el nombre de las variables
names(Smarket)
# para ver las dimensiones
dim(Smarket)
# para tener un resumen del dataset
summary(Smarket)

# para generar una matriz que contiene todos las correlaciones entre los predictores en un conjunto de datos.
# todas las variables deben ser cuantitativas, sino da error. Hay que eliminar "Direction que es cualitativa"
cor(Smarket[, -9])
# una correlacion de 0 significa que dichas variables tienen poca correlacion. 

attach(Smarket)
plot(Volume)

# LOGISTIC REGRESION
# modelo de regresión logística para predecir la "Dirección" usando Lag1 a Lag5 y Volumen. 
# La función glm() se usa para ajustar muchos tipos de modelos lineales generalizados, incluida la regresión logística. 
# Se debe pasar el argumento family = binomial para decirle a R que ejecute una regresion logística
glm.fits = glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, data = Smarket , family = binomial)

summary(glm.fits)
# El valor p más pequeño aca está asociado con Lag1 (0.145). 
# El coeficiente negativo de este predictor (-0.073) sugiere que si el mercado tuvo un rendimiento positivo ayer,
# entonces es menos probable que suba hoy. Sin embargo, a un valor de p de 0,145, 
# todavía es relativamente grande, por lo que no hay evidencia clara de una asociación real entre Lag1 y Dirección.

#Usamos la función coef() para acceder solo a los coeficientes para este modelo ajustado. 
coef(glm.fits)

#También podemos utilizar la función summary() para acceder a determinados aspectos del modelo ajustado, 
#como los valores p para los coeficientes.
summary(glm.fits)$coef

summary(glm.fits)$coef[, 4]

# La función predict() se puede utilizar para predecir la probabilidad de que el mercado subirá, 
# dados los valores de los predictores. La opcion type = "response" le dice a R que genere 
# probabilidades de la forma P(Y = 1|X), a diferencia de otra información como el logit. 
# Si no se proporciona ningún conjunto de datos a la función predict (), luego se calculan las probabilidades 
# para los datos de entrenamiento que se utilizaron para ajustar el modelo de regresión logística.
glm.probs <- predict(glm.fits , type = "response")
# se imprimen solo las 10 primeras probabilidades
glm.probs[1:10]

# Sabemos que estos valores corresponden a la probabilidad de que el mercado suba, en lugar de bajar, 
# porque la funcion contrasts() indica que R ha creado una variable ficticia con un 1 para subir
contrasts(Direction)

# Para hacer una predicción sobre si el mercado subirá o bajara en un día en particular, 
# debemos convertir estas probabilidades predichas en etiquetas de clase, Arriba o Abajo. 
# Los siguientes dos comandos crean un vector de predicciones de clase basadas 
# en si la probabilidad predicha de aumento de un mercado es mayor o menor que 0.5.
# El primer comando crea un vector de 1250 elementos Down.
glm.pred <- rep("Down", 1250)
# transforma en Up todos los elementos para los cuales la probabilidad predicha de aumento del mercado supera el 0,5.
glm.pred[glm.probs > .5] = "Up"

# la función tabla() se puede utilizar para producir una MATRIZ DE CONFUSION con el fin de determinar cuántas
# observaciones fueron clasificadas correcta o incorrectamente.
table(glm.pred , Direction)
# Los elementos diagonales de la matriz de confusión indican predicciones correctas,
# De ahí nuestro modelo predijo correctamente que el mercado subiría en 507 días y que bajaría en 145 días, 
# para un total de 507 + 145 = 652 correcto predicciones.

# La función mean() se puede utilizar para calcular la fracción de días para los que 
# la predicción fue correcta. 
mean(glm.pred == Direction)
# En este caso, la regresión logística predijo correctamente el movimiento del mercado el 52,2% de las veces.

# A primera vista, parece que el modelo de regresión logística está funcionando un poco mejor 
# que adivinar al azar. Sin embargo, este resultado es engañoso porque entrenamos y probamos el modelo 
# en el mismo conjunto de 1250 observaciones.
# En otras palabras, 100% − 52,2% = 47,8%, es el error de entrenamiento.
# Para mejor evaluar la precisión del modelo de regresión logística en este escenario, podemos ajustar el modelo 
# usando parte de los datos y luego examinar qué tan bien predice los datos retenidos. 
# Esto producirá una tasa de error más realista, en el sentido que en la práctica estaremos interesados 
# en el rendimiento de nuestro modelo no en los datos que usamos para ajustar el modelo, sino en días en el futuro 
# para lso cuales se desconocen los movimientos del mercado.

# creamos un vector correspondiente a las observaciones de 2001 a 2004.
train <- (Year < 2005)
# El objeto train es un vector de 1.250 elementos, correspondiente a las observaciones
# en nuestro conjunto de datos. Train es un Vector booleano, ya que sus elementos son VERDADERO y FALSO.
Smarket.2005 <- Smarket[!train, ]
# Smarket[!train, ] produce una submatrizde los datos bursátiles que contengan únicamente las observaciones
# para las que train es FALSO, es decir, las observaciones con fechas en 2005.
dim(Smarket.2005)
# hay 252 observaciones, todos los predictores
Direction.2005 <- Direction[!train]

# Ahora ajustamos un modelo de regresión logística usando solo el subconjunto de las observaciones
# que corresponden a fechas anteriores a 2005, utilizando el argumento de subconjunto.
glm.fits <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume ,
                data = Smarket , family = binomial , subset = train)
# We then obtain predicted probabilities of the stock market going up for
# each of the days in our test set—that is, for the days in 2005.
glm.probs <- predict(glm.fits , Smarket.2005, type = "response")

# Finally, we compute the predictions for 2005 and compare them to the actual movements
# of the market over that time period.
glm.pred <- rep("Down", 252)
glm.pred[glm.probs > .5] <- "Up"
table(glm.pred , Direction.2005)
mean(glm.pred == Direction.2005)
# para computar el TEST ERROR RATE, hago el not equal to
mean(glm.pred != Direction.2005)
# el TEST ERROR RATE es del 52 %, ¡lo cual es peor que adivinar al azar! Por supuesto este resultado
# no es tan sorprendente, dado que uno generalmente no esperaría ser capaz de utilizar 
# los rendimientos de días anteriores para predecir el rendimiento futuro del mercado.

# Recordamos que el modelo de regresión logística tenía valores p muy decepcionantes
# asociado con todos los predictores, y que el valor p más pequeño, aunque no muy pequeño, 
# correspondió a Lag1. Quizá quitando las variables que parecen no ser útiles para predecir la Dirección, 
# podemos obtener un modelo más eficaz. Después de todo, usar predictores que no tienen
# relación con la respuesta tienden a provocar un deterioro en la TEST ERROR RATE 
# (ya que tales predictores causan un aumento en la varianza sin una disminución correspondiente en el sesgo), 
# por lo que la eliminación de tales predictores puede en a su vez producir una mejora. 

# A continuación hemos reajustado la regresión logística usando sólo Lag1 y Lag2, (ME GUSTAN LOS P CHICOS)
# que parecían tener el mayor poder predictivo en el modelo de regresión logística original.
glm.fits <- glm(Direction ~ Lag1 + Lag2 , data = Smarket, family = binomial , subset = train)
glm.probs <- predict(glm.fits , Smarket.2005, type = "response")
glm.pred <- rep("Down", 252)
glm.pred[glm.probs > .5] <- "Up"
table(glm.pred , Direction.2005)
mean(glm.pred == Direction.2005)
106 / (106 + 76)
# However,the confusion matrix shows that on days when logistic regression predicts
# an increase in the market, it has a 58% accuracy rate.

# Supongamos que queremos predecir los rendimientos asociados con determinados valores de Lag1 y Lag2. 
# En particular, queremos predecir la Dirección en un día en que Lag1 y Lag2 son iguales a 1,2 y 1,1, respectivamente, 
# y en un día en que son iguales a 1,5 y −0,8. Hacemos esto usando la función predecir().
predict(glm.fits,
        newdata = data.frame(Lag1 = c(1.2 , 1.5) , Lag2 = c(1.1 , -0.8)),
        type = "response")

# ---------------------------------------------------------------------------------------------------------------------

#OTRA FORMA DE PARTIR LA MUESTRA PARA TRAIN Y TEST:
library(caret)
trainIndex<-createDataPartition(Direction,p=0.8,list=FALSE,times=1)
#como primer argumento de la función "createDataPartition" ponemos la variable que queremos predecir,
#en segundo lugar el porcentaje de datos que queremos usar para el train, el tercer
#argumento se pone FALSE para que no me lo convierta en lista.
Smarket_train<-Smarket[trainIndex,]
Smarket_test<-Smarket[-trainIndex,]
glm.fit<-glm(Direction~Lag1+Lag2, data = Smarket_train, family = binomial)
glm.probs<-predict(glm.fit,Smarket_test,type="response")
glm.pred<-rep("Down",249)
glm.pred[glm.probs>.5]<-"Up"
table(glm.pred,Smarket_test$Direction)
mean(glm.pred==Smarket_test$Direction)
mean(glm.pred!=Smarket_test$Direction)

# ---------------------------------------------------------------------------------------------------------------------

# ROC CURVE
# documentacion: https://www.rdocumentation.org/packages/pROC/versions/1.18.0/topics/roc
# ejemplo: https://daviddalpiaz.github.io/r4sl/logistic-regression.html#roc-curves

library(pROC)
glm.probs = predict(glm.fit,Smarket_test,type="response")
glm_roc = roc(Smarket_test$Direction ~ glm.probs, plot = TRUE, print.auc = TRUE )

#test_prob = predict(model_glm, newdata = default_tst, type = 'response')
#test_roc = roc(default_tst$default ~ test_prob, plot = TRUE, print.auc = TRUE)


# ---------------------------------------------------------------------------------------------------------------------

# LINEAR DISCRIMINAN ANALYSIS (LDA)

# En R ajustamos un modelo LDA utilizando la función lda(), que forma parte de la biblioteca MASS. 
# La sintaxis de la función lda() es idéntica a la deglm() excepto por la ausencia de la opción de familia. 
# Ajustamos el modelo usando sólo las observaciones anteriores a 2005.

library(MASS)
lda.fit <- lda(Direction ~ Lag1 + Lag2 , data = Smarket, subset = train)
lda.fit
plot(lda.fit)
" La salida LDA indica que π1 = 0.492 y π2 = 0.508; en otras palabras,el 49,2% de 
las observaciones de entrenamiento corresponden a días en los que el mercado bajó. 
También proporciona las medias del grupo; estos son el promedio de cada predictor dentro de cada clase, 
y son utilizados por LDA como estimaciones de μk. Estos sugieren que hay una tendencia de los 2 días anteriores a
ser negativos en los días en que el mercado sube, y una tendencia para el los rendimientos de los 2 dias anteriores 
a ser positivos en los días en que el mercado cae. Los coeficientes de salida de discriminantes lineales proporcionan
la combinación lineal de Lag1 y Lag2 que se utilizan para formar la regla de decisión LDA. 
Si −0,642 × Lag1 − 0,514 × Lag2 es grande, entonces el clasificador LDA predecirá un aumento del mercado, 
y si es pequeño, entonces el clasificador LDA predecirá un caída del mercado.
La función plot() produce gráficos de los discriminantes lineales, obtenidos
calculando −0.642×Lag1−0.514×Lag2 para cada una de las observaciones de entrenamiento.
Las observaciones de Arriba y Abajo se muestran por separado."

" La función predecir() devuelve una lista con tres elementos. El primer elemento,
clase, contiene las predicciones de LDA (DOWN/ UP) sobre el movimiento del mercado para cada 250 observacion.
El segundo elemento, posterior, es una matriz cuya columna k-ésima contiene la
probabilidad a posteriori de que la observación correspondiente pertenezca a la k-ésima
clase. Finalmente, x contiene los discriminantes lineales, descritos anteriormente."
lda.pred <- predict(lda.fit , Smarket.2005)
names(lda.pred)
lda.class <- lda.pred$class
table(lda.class, Direction.2005)
mean(lda.class == Direction.2005)
" the LDA and logistic regression predictions are almost identical"
lda.class

" Aplicar un umbral del 50% a las probabilidades a posteriori nos permite recrear
las predicciones contenidas en lda.pred$class."
sum(lda.pred$posterior[, 1] >= .5)
sum(lda.pred$posterior[, 1] < .5)
" Escrita de esta forma la salida de probabilidad posterior del modelo corresponde 
a la probabilidad de que el mercado disminuya"
lda.pred$posterior[1:20, 1]
lda.class[1:20]

"Si quisiéramos usar un umbral de probabilidad posterior diferente al 50%
para hacer predicciones, entonces podríamos hacerlo fácilmente. Por ejemplo, supongamos
que deseamos predecir una disminución del mercado solo si estamos muy seguros de que
el mercado disminuirá ese día, es decir, si la probabilidad posterior es al menos el 90 %."
sum(lda.pred$posterior[, 1] > .9)


# ---------------------------------------------------------------------------------------------------------------------

# QUADRATIC DISCRIMINAN ANALYSIS (QDA)

"QDA está implementado en R utilizando la función qda(), que también forma parte 
de la biblioteca MASS. La sintaxis es idéntica a la de lda()."
qda.fit <- qda(Direction ~ Lag1 + Lag2 , data = Smarket, subset = train)
qda.fit
"La salida contiene las medias del grupo. Pero no contiene los coeficientes
de los discriminantes lineales, porque el clasificador QDA implica un
función cuadrática, en lugar de lineal, de los predictores. La predicción ()
funciona exactamente de la misma manera que para LDA."
qda.class <- predict(qda.fit , Smarket.2005)$class
table(qda.class , Direction.2005)
mean(qda.class == Direction.2005)
"Curiosamente, las predicciones de QDA son precisas casi el 60% del tiempo,
a pesar de que los datos de 2005 no se utilizaron para ajustar el modelo. Este nivel de precisión
es bastante impresionante para los datos del mercado de valores, que se sabe que son bastante
difícil de modelar con precisión. Esto sugiere que la forma cuadrática asumida
por QDA puede capturar la verdadera relación con mayor precisión que las formas
linealies asumidas por LDA y regresión logística."

# ---------------------------------------------------------------------------------------------------------------------

# NAIVE BAYES
"Naive Bayes se implementa en R usando la función naiveBayes(), que es parte de
la biblioteca e1071. La sintaxis es idéntica a la de lda() y qda(). Por defecto, 
la implementacion del clasificador Bayes modela cada característica cuantitativa
utilizando una distribución gaussiana. Sin embargo, un método de densidad kernel puede
también se puede utilizar para estimar las distribuciones."
library(e1071)
nb.fit <- naiveBayes(Direction ~ Lag1 + Lag2 , data = Smarket, subset = train)
nb.fit
"La salida contiene la media estimada (primer columna) y la desviación estándar 
(segunda columna) para cada variable en cada clase. 
Por ejemplo, la media de Lag1 es 0.0428 para Dirección=Down, y la desviación estándar es 1.23
Esto se puede verificar fasilmente"
mean(Lag1[train][Direction[train] == "Down"])
sd(Lag1[train][Direction[train] == "Down"])

"La función predict() es sencilla:"
nb.class <- predict(nb.fit , Smarket.2005)
table(nb.class , Direction.2005)
mean(nb.class == Direction.2005)
"Naive Bayes funciona muy bien con estos datos, con predicciones precisas sobre
59% del tiempo. Esto es ligeramente peor que QDA, pero mucho mejor que
LDA.

La función predecir () también puede generar estimaciones de la probabilidad
que cada observación pertenece a una clase particular."
nb.preds <- predict(nb.fit , Smarket.2005, type = "raw")
nb.preds[1:5, ]

# ---------------------------------------------------------------------------------------------------------------------

# K-NEAREST NEIGHBORS (KNN)

 "En lugar de un enfoque en 2 pasos donde en el que primero ajustamos el modelo y luego usamos el modelo para hacer
predicciones, knn() forma predicciones usando un solo comando. 
La función requiere cuatro entradas.
1. Una matriz que contiene los predictores asociados con los datos de entrenamiento, etiquetado train.X 
2. Una matriz que contiene los predictores asociados con los datos para los cuales deseamos hacer predicciones, etiquetadas como test.X 
3. Un vector que contiene las etiquetas para las observaciones de entrenamiento, train.Directions
4. Un valor para K, el número de vecinos más cercanos que utilizará el clasificador

Usamos la función cbind(), para enlazar las variables Lag1 y Lag2 en dos matrices, una para el conjunto de entrenamiento y la
otro para el conjunto de prueba." 
"(recordamos que train ya estaba definido por los valores de la columna Year < 2005)"
library(class)
train.X <- cbind(Lag1 , Lag2)[train , ]
test.X <- cbind(Lag1 , Lag2)[!train , ]
train.Direction <- Direction[train]

"Ahora la función knn() se puede usar para predecir el movimiento del mercado para
las fechas en 2005. Establecemos una RANDOM SEED antes de aplicar knn() porque si varias observaciones están empatadas 
como vecinos más cercanos, entonces R rompe el empate aleatoriamente. Por lo tanto, se debe establecer una semilla 
para garantizar la reproducibilidad de resultados"

set.seed (1)
knn.pred <- knn(train.X, test.X, train.Direction , k = 1)
table(knn.pred , Direction.2005)
(83 + 43) / 252
"El porcentaje de observaciones bien clasificadas es del 50%"
"Los resultados usando K = 1 no son muy buenos, ya que solo el 50% de las observaciones se predicen correctamente. 
Por supuesto, puede ser que K = 1 resulte en un ajuste excesivamente flexible a los datos. 
A continuación, repetimos el análisis usando K = 3."
knn.pred <- knn(train.X, test.X, train.Direction , k = 3)
table(knn.pred , Direction.2005)
mean(knn.pred == Direction.2005)
"Los resultados han mejorado ligeramente. Pero aumentar mas K, no mejora los resultados"

"CON OTRO SET DE DATOS"
"Como ejemplo, aplicaremos el enfoque KNN al conjunto de datos de Caravan, que forma parte de la biblioteca ISLR2. 
Este conjunto de datos incluye 85 predictores que miden las características demográficas de 5.822 individuos.
La variable de respuesta es Compra, que indica si un determinado individuo compra una póliza de seguro de caravana. 
En este conjunto de datos, sólo el 6% de las personas compraron un seguro de caravana."
dim(Caravan)
attach(Caravan)
summary(Purchase)

"Debido a que el clasificador KNN predice la clase de una observación dada por las observaciones más cercanas a él, 
la escala de las variables importa. Las variables que son de gran escala tendrán un efecto mucho mayor
en la distancia entre las observaciones, y por lo tanto en el clasificador KNN, que las variables que son 
de pequeña escala. Por ejemplo, un conjunto de datos que contiene dos variables, salario y edad (medida en dólares y años,
respectivamente). En lo que respecta a KNN, una diferencia de $1,000 en salario es enorme en comparación 
con una diferencia de 50 años de edad. Como consecuencia, salario impulsará los resultados de la clasificación KNN, 
y la edad tendrá casi sin efecto. Esto es contrario a nuestra intuición de que una diferencia salarial de $1,000
es bastante pequeña en comparación con una diferencia de edad de 50 años. Además, La importancia de la escala 
para el clasificador KNN lleva a otro problema: si salario medido en yenes japoneses, o si medimos la edad en minutos, 
entonces obtendríamos resultados de clasificación bastante diferentes de los que obtenemos si estas 2 variables 
se miden en dólares y años. Una buena manera de manejar este problema es ESTANDARIZAR los datos para que todas las 
variables tengan una media de cero y una desviación estándar de uno. Después todas las variables estarán 
en una ESCALA COMPARABLE. La función scale() solo hace esto. 
Al estandarizar los datos, excluimos la columna 86, porque esa es la variable cualitativa Compra."
standardized.X <- scale(Caravan[, -86])
var(Caravan[, 1])
var(Caravan[, 2])
var(standardized.X[, 1])
var(standardized.X[, 2])
"Ahora cada columna de standarized.X tiene una desviación estándar de uno y una media de cero.

Ahora dividimos las observaciones en un conjunto de prueba, que contiene los primeros 1000
observaciones, y un conjunto de entrenamiento, que contiene las observaciones restantes.
Ajustamos un modelo KNN con los datos de entrenamiento usando K = 1, y evaluamos su rendimiento en los datos de prueba."
test <- 1:1000
"Se crea un vector numerico, con valores de 1 a 1, 000"
train.X <- standardized.X[-test, ]
test.X <- standardized.X[test, ]
train.Y <- Purchase[-test]
test.Y <- Purchase[test]
"Con standarized.X[test, ] produce la submatriz de los datos que contienen las observaciones cuyos índices 
oscilan entre 1 y 1.000, mientras que escribir standarized.X[-test, ] produce la submatriz que contiene las observaciones
cuyos índices no van de 1 a 1.000."
set.seed (1)
knn.pred <- knn(train.X, test.X, train.Y, k = 1)
mean(test.Y != knn.pred)
"El ERROR RATE KNN en el 1000 observaciones de prueba es poco menos del 12%."
mean(test.Y != "No")

"En cambio, la fracción de individuos que están correctamente pronosticados para comprar un seguro es de interés."
table(knn.pred , test.Y)
9 / (68 + 9)
"Usando K = 3, la tasa de éxito aumenta al 19 %, y con K = 5 la tasa es 26,7 %."
knn.pred <- knn(train.X, test.X, train.Y, k = 3)
table(knn.pred , test.Y)
5 / 26
knn.pred <- knn(train.X, test.X, train.Y, k = 5)
table(knn.pred , test.Y)
4 / 15

"Como comparación, también podemos ajustar un modelo de REGRESION LOGISTICA a los datos.
Si usamos 0.5 como el corte de probabilidad pronosticado para el clasificador, entonces tenemos un problema: 
se predice que solo siete de las observaciones de prueba compran el seguro. Peor aún, 
¡nos equivocamos en todo esto! Sin embargo, no estamos obligados a utilizar un límite de 0,5. 
Si en cambio predecimos una compra cada vez que la probabilidad de compra predicha excede 0.25, obtenemos mucho
mejores resultados: predecimos que 33 personas comprarán un seguro, y son correctos para alrededor del 33% de estas personas."
glm.fits <- glm(Purchase ~ ., data = Caravan, family = binomial , subset = -test)
glm.probs <- predict(glm.fits, Caravan[test , ], type = "response ")
glm.pred <- rep("No", 1000)
glm.pred[glm.probs > .5] <- "Yes"
table(glm.pred , test.Y)

glm.pred <- rep("No", 1000)
glm.pred[glm.probs > .25] <- "Yes"
table(glm.pred , test.Y)
11 / (22 + 11)