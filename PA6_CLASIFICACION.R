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
glm.fits = glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume ,
  data = Smarket , family = binomial)

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
# Si no se proporciona ningún conjunto de datos a la función predict (), 
# luego se calculan las probabilidades para los datos de entrenamiento que se utilizaron para ajustar el modelo de regresión logística.
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
# Para mejorevaluar la precisión del modelo de regresión logística en este escenario, podemos ajustar el modelo 
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