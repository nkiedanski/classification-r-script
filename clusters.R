Para encontrar el número óptimo de clusters utilzamos el paquete NbClust
que contiene el cálculo de 30 índices todos a la vez, además me resume la información
de la cantidad de índices que coinciden dando el número óptimo de clusters en cada caso.


library("NbClust")
Por ejemplo ¿en cúantos grupos será conveniente  separar en clusters los
estados de USA del conjunto de datos USArrests?

arrestos= scale(USArrests)##estandarizamos los datos para quitarles la unidad de medida

resumen_k_optimo <- NbClust(arrestos, distance = "euclidean", min.nc = 2,
max.nc = 10, method = "kmeans", index = "alllong")

Graficamos el coeficiente de Silhouette

t=seq(2,10)##10 es el número máximo de clusters que pusimos, podría ser otro
plot(t,resumen_k_optimo$All.index[,13],type="o",xlab="number of clusters", 
ylab="",main="Silhouette")

resumen_k_optimo$Best.nc

sum(resumen_k_optimo$Best.nc[1,]==2)
sum(resumen_k_optimo$Best.nc[1,]==3)
sum(resumen_k_optimo$Best.nc[1,]==4)
sum(resumen_k_optimo$Best.nc[1,]==5)
sum(resumen_k_optimo$Best.nc[1,]==6)
sum(resumen_k_optimo$Best.nc[1,]==7)
sum(resumen_k_optimo$Best.nc[1,]==8)
sum(resumen_k_optimo$Best.nc[1,]==9)
sum(resumen_k_optimo$Best.nc[1,]==10)


####################################################################
HEATMAPS
Es la combinación de un dendrograma con la matriz de datos, cambiando 
los números de la matriz por colores. Cuanto más parecidos son dos individuos
en determinada variable, más parecidos deberían ser los correspondientes colores
a lo largo de toda la fila. Cuanto más fuerte el color, más pequeño el valor de la 
variable (aunque esto no es relevante).
Ver por ejemplo 

arrestos[28,]##Nevada
arrestos[29,]##New Hampshire

heatmap(x = scale(USArrests), scale = "none",
distfun = function(x){dist(x, method = "euclidean")},
hclustfun = function(x){hclust(x, method = "ward.D2")},
cexRow = 0.7)

Para dibujar el dendrograma, hay que darle la matriz de distancias.

arrestos=scale(USArrests)


hc=hclust(dist(arrestos),"ward.D2")
plot(as.dendrogram(hc))




