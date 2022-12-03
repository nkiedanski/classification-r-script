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
biplot(pr.out,scale=0) # se pone scale=0 para asegurar que las flechas están escaladas para representar las cargas
"ANALISIS: 
- los puntos más cercanos al (0,0) son los que tienen valores más parecidas al promedio
- mayor ángulo mayor incorrelación, a menor ángulo más correlación. 
- Ángulo recto significa incorrelacionadas."

"Algunos datos que nos pueden interesar de cada componente principal"
pr.out$sdev # desviacion estandar
pr.out$sdev^2 # varianza
pve <- summary(pr.out)$importance[2,] # proporción de la varianza explicada por cada componente principal
pve

#plot de la proporcion de varianza explicada
plot(pve , xlab=" Principal Component ", ylab="Proportion of Variance Explained ", ylim=c(0,1),type='b') 

#plot de la proporcion de varianza acumulada
plot(cumsum(pve), xlab="Principal Component ", ylab="Cumulative Proportion of Variance Explained ", ylim=c(0,1),type='b')
# Interesa donde se forma el codo

