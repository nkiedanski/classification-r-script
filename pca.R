rm(list=ls())
library(ISLR2)

# ANALISIS DEL DATASET
data("USArrests")
summary(USArrests)
head(USArrests)
names(USArrests) # puede haber data y etiquetas

#nci.labs <- NCI60$labs
#nci.data <- NCI60$data
dim(USArrests)
table(USArrests) # es para hacer un especie de groupby()

# PCA 
pr.out <- prcomp(USArrests , scale = TRUE) # con escalado de datos cuando haya diferentes medidas
pr.out
pr.out$rotation # las columnas continen los vectores propios

summary(pr.out) # la importancia de cada componente
biplot(pr.out,scale=0) # se grafican las primeras 2 componentes principales. 
#Se pone scale=0 para asegurar que las flechas están escaladas para representar las cargas

"ANALISIS: 
- los puntos más cercanos al origen (0,0) son los que tienen valores más parecidas al promedio, poco llamativos.
- Cuanto mas chiquito sea el angulo, mas se correlacionan entre si, ademas como la correlacion es positiva
cuando una aumenta la otra tambien aumenta.
- Ángulo recto significa que las variables no se correlacionan entre si, 
- Cuando el angulo es mas que recto, son bastante independientes y ademas como la correlacion es negativa, 
cuando una aumenta, la otra disminuye,
- Cuando estan una sobre el otro, es que tienen la misma influencia."

"Algunos datos que nos pueden interesar de cada componente principal"
pr.out$sdev # desviacion estandar
pr.out$sdev^2 # varianza
pve <- summary(pr.out)$importance[2,] # proporción de la varianza explicada por cada componente principal
pve

#plot de la proporcion de varianza explicada
plot(pve , xlab=" Principal Component ", ylab="Proportion of Variance Explained ", ylim=c(0,1),type='b') 

#plot de la proporcion de varianza acumulada
plot(cumsum(pve), xlab="Principal Component ", ylab="Cumulative Proportion of Variance Explained ", ylim=c(0,1),type='b')


"Criterios para calcular la cantidad de componentes principales:
La idea es mirar el Cumulative proportion de las dos primeras componentes 
(o sea el segundo lugar del vector Cumulative proportion). Si ese valor es muy alto, por ejemplo 99%, 
quiere decir que explicás casi toda la variabilidad con solo las dos componentes principales 
que son las que aparecen después en el biplot, entonces en ese caso lo que ves en el biplot es recontra informativo, 
sin embargo si la variabilidad explicada fuera del 50% por ejemplo, entonces el biplot no te dice tanto 
en cuanto a las conclusiones porque estás mirando sólo dos componentes que explican apenas la mitad."
