#CLUSTERING

#separar en grupos por KMEANS y por PAM EL SIGUIENTE SET DE DATOS:

set.seed(2)
x<-matrix(rnorm(50*2),ncol=2)
#ahora desfaso 2 grupos de 25 datos
x[1:25,1]=x[1:25,1]+3
x[1:25,2]=x[1:25,2]-4


#primero estandarizamos los datos para quitarles la unidad de medida
matrizestand<-scale(x)

#APLICAMOS K-MEANS
library("NbClust")
library("factoextra")
resumen_k_optimo<-NbClust(matrizestand,
                          distance="euclidean",
                          min.nc=2,
                          max.nc=10,
                          method="kmeans",
                          index="alllong")
#al poner "alllong" calculo todos los indices (incluye Silhouette y Dunn)
#ver que output arroja la "conclusion"
#coloco el valor sugerido de k en la siguiente función de kmeans
km.res<-kmeans(matrizestand,2,nstart=20) 
#con la siguiente función podemos visualizar gráficamente como quedan los clusters con K-MEANS
fviz_cluster(km.res,
             data=matrizestand,
             stand=FALSE,
             ellipse=TRUE,
             xlab=FALSE,
             ylab=FALSE,
             ggtheme=theme_minimal())

#OTRA FORMA DE APLICAR K-MEANS Y VISUALIZAR EL NÚMERO ÓPTIMO DE CLUSTERS
#método del codo
fviz_nbclust(matrizestand,kmeans,method="wss")+geom_vline(xintercept=2,linetype=2)
#silhouette
fviz_nbclust(matrizestand,kmeans,method="silhouette")
#ver que número óptimo de clusters sugieren los gráficos

#APLICAMOS PAM
library(cluster)

fviz_nbclust(matrizestand,cluster::pam,method="silhouette")+theme_minimal()
#ver que sugiere el gráfico como número óptimo de clusters, e ingresarlo como segundo argumento en
#la siguiente función

pamResult<-pam(matrizestand,2,metric="euclidean",nstart=20,stand=FALSE) 
#FALSE xq ya estan estandarizados los datos. Lo hice para k=2, en caso de que fueran más debemos
#incluir más colores o borrar ese argumento
fviz_cluster(pamResult, 
            palette="Set2",
            ellipse.type="euclid",
            repel=TRUE,
            xlab=FALSE,
            ylab=FALSE,
            ggtheme=theme_minimal())


#calcular coeficientes de Silhouette y Dunn o algún otro

#en los casos de abajo evaluo de 2 a 10 clusters con kmeans
#Silhouette
Silhouette_k_optimo<-NbClust(matrizestand,
                             distance="euclidean",
                             min.nc=2,
                             max.nc=10,
                             method="kmeans",
                             index="silhouette")
Silhouette_k_optimo$Best.nc

#Dunn
Dunn_k_optimo<-NbClust(matrizestand,
                       distance="euclidean",
                       min.nc=2,
                       max.nc=10,
                       method="kmeans",
                       index = "dunn")
Dunn_k_optimo$Best.nc

#Construir dendrogramas con distintas medidas de distancias y heatmaps

heatmap(matrizestand,
        scale="none",
        distfun=function(x){dist(x,method="euclidean")},
        hclustfun=function(x){hclust(x,method="ward.D2")},
        cexRow=0.7)

#Para dibujar el dendrograma, hay que darle la matriz de distancias.

hc=hclust(dist(matrizestand),"ward.D2")
plot(as.dendrogram(hc))

#lo siguiente lo dejo por las dudas que quiera algún otro dendrograma
hc.complete<-hclust(dist(matrizestand),method="complete")
hc.average<-hclust(dist(matrizestand),method="average")
hc.single<-hclust(dist(matrizestand),method="single")

#ahora se plotean los dendrogramas y se usa la función par() para dividir la pantalla en 
#paneles diferentes para que se puedan ver múltiples plots al mismo tiempo
par(mfrow=c(1,3))
plot(hc.complete,main="Complete Linkage",xlab="",sub="",cex=9)
plot(hc.average,main="Average Linkage",xlab="",sub="",cex=9)
plot(hc.single,main="Single Linkage",xlab="",sub="",cex=9)
