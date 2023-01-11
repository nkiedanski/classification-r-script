music_africa
music_africa_var = music_africa[,-117] #saco la etiqueta que no aplica
music_africa_var150.10 = music_africa_var[1:150, 1:10]# 150 observaciones las primeras 10 variables
music_africa_var150.10

library("NbClust")           #para hallar la cantidad optima de grupos
library("factoextra")        #para extraer y visualizar output
library(clv)                 #para calcular algunos indices


#PARTE 1
resumen_k_optimo<-NbClust(music_africa_var150.10,
                          distance="euclidean",
                          min.nc=2,
                          max.nc=10,
                          method="kmeans",
                          index="all") # se calculan todos los indices (incluye Silhouette y Dunn)

#PARTE 2
hc=hclust(dist(music_africa_var150.10), method="ward.D2") #dist() porque hay que trabajar con distancias, se usa la Euclideana por default
plot(as.dendrogram(hc), main = 'Dendograma', xlab = '', sub = '', ylab = '')

#PARTE 3 #no ploteo
music_africa_var50.10 = music_africa_var[1:50,1:10]
heatmap(dist(music_africa_var50.10),
        scale="none",
        distfun=function(x){dist(x,method="euclidean")},
        hclustfun=function(x){hclust(x,method="ward.D2")},
        cexRow=0.7)
