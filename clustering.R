rm(list=ls())
library("NbClust")           #para hallar la cantidad optima de grupos
library("factoextra")        #para extraer y visualizar output
library(clv)                 #para calcular algunos indices

data(LifeCycleSavings)
summary(LifeCycleSavings)
head(LifeCycleSavings)
names(LifeCycleSavings) # puede haber data y etiquetas

#nci.labs <- NCI60$labs
#nci.data <- NCI60$data
dim(LifeCycleSavings)
table(LifeCycleSavings) # es para hacer un especie de groupby()

scaled_data = scale(LifeCycleSavings) #se escalan los datos
head(scaled_data)

################################ K-MEANS #######################################

resumen_k_optimo<-NbClust(scaled_data,
                          distance="euclidean",
                          min.nc=2,
                          max.nc=10,
                          method="kmeans",
                          index="all") # se calculan todos los indices (incluye Silhouette y Dunn)
"Se elige el mejor k de acuerdo a la conclusion"
km.res <- kmeans(scaled_data, centers=2 , nstart=20) 
km.res
km.res$cluster

"se visualiza como quedan los clusters"
fviz_cluster(km.res,
             data=scaled_data,
             stand=FALSE,
             ellipse=TRUE, #si es VERDADERO, dibuja el contorno alrededor de los puntos de cada grupo
             xlab=FALSE,
             ylab=FALSE,
             ggtheme=theme_gray())

"Otras formas de determinar y visualiza el número óptimo de grupos"
fviz_nbclust(scaled_data,kmeans,method="wss")+geom_vline(xintercept=2,linetype=2) #inercia intra clase (suma de cuadrados), interesa que sea minima
fviz_nbclust(scaled_data,kmeans,method="silhouette") #silhouette, interesa que sea grande

"Calculo del coeficiente de silhouette"
Silhouette_k_optimo<-NbClust(scaled_data,
                             distance="euclidean",
                             min.nc=2,
                             max.nc=10,
                             method="kmeans",
                             index="silhouette")
Silhouette_k_optimo$Best.nc
"> 0.7 fuerte
 entre 0.5 y 0.7 razonable
 entre 0.25 y 0.5 debil
 < 0.25 no hay una estructura clara de grupos"

"Calculo del coeficiente de Dunn"
Dunn_k_optimo<-NbClust(scaled_data,
                       distance="euclidean",
                       min.nc=2,
                       max.nc=10,
                       method="kmeans",
                       index = "dunn")
Dunn_k_optimo$Best.nc


"para comparar el clustering verdadero con el que predice el algoritmo ( o comprar el output de 2 algoritmos)"
#std <- std.ext(km.res$cluster, km.res$cluster)

#rand1 <- clv.Rand(std)
#print(rand1)
#jaccard1 <- clv.Jaccard(std)
#print(jaccard1)
#folk.mal1 <- clv.Folkes.Mallows(std)
#print(folk.mal1)

################################ PAM - PARTICIONAMIENTO ########################

fviz_nbclust(scaled_data,cluster::pam,method="silhouette")+theme_minimal() #evaluacion no. optimo de grupos con silhouette
pamResult<-pam(scaled_data, k=2, metric="euclidean",nstart=20,stand=FALSE) #los datos ya estan estandarizados
fviz_cluster(pamResult,          #para visualizar
             palette="Set2",
             ellipse=TRUE,
             repel=TRUE, #si usar ggrepel para evitar sobretrazar las etiquetas de texto o no.
             xlab=FALSE,
             ylab=FALSE,
             ggtheme=theme_gray())

################################ HEATMAP #######################################

heatmap(scaled_data,
        scale="none",
        distfun=function(x){dist(x,method="euclidean")},
        hclustfun=function(x){hclust(x,method="ward.D2")},
        cexRow=0.7)

################################ DENDOGRAMAS ###################################

#se arma el dendrograma jerarquico

hc=hclust(dist(scaled_data), method="ward.D2") #dist() porque hay que trabajar con distancias, se usa la Euclideana por default
plot(as.dendrogram(hc), main = 'Dendograma', xlab = '', sub = '', ylab = '')
abline(h = 6, col = "red") #se puede trasar una linea

"Se pueden considerar otras distancias"
hc.complete<-hclust(dist(scaled_data),method="complete") #suele separar bien
hc.average<-hclust(dist(scaled_data),method="average") #suele separar bien
hc.single<-hclust(dist(scaled_data),method="single")

hc.clusters = cutree(hc , 2) #se puede cortar el dendograma tal que forme 2 grupos
hc.clusters
