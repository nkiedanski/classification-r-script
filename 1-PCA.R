#ANÁLISIS DE COMPONENTES PRINCIPALES (PCA)
#si tengo que crear la matriz, primero creo los vectores columna de la siguiente forma
columna1<-c(1,2,3,4,5,6,7,8,9)
columna2<-c(2,3,4,5,6,7,8,9,1)
columna3<-c(3,4,5,6,7,8,9,1,2)
columna4<-c(4,5,6,7,8,9,1,2,3)
columna5<-c(5,6,7,8,9,1,2,3,4)
#a partir de las columnas creamos la matriz (sin nombres en las filas)
matrizdatos<-matrix(c(columna1,columna2,columna3,columna4,columna5),
              nrow=9,
              ncol=5)


#si quiero agregar nombres a las columnas hago lo siguiente:
colnames(matrizdatos)<-c('columna1','columna2','columna3','columna4','columna5')
#si quiero agregar nombres a las filas hago lo siguiente:
rownames(matrizdatos)<-c('fila1','fila2','fila3','fila4','fila5','fila6','fila7','fila8','fila9')


#si quiero hacer una primera comparación entre columnas, calculo los promedios con la función "apply"
#donde primero le damos el nombre de la matriz, y luego un 2 porque quiero que haga promedio en 
#las columnas (si fuese un 1 se lo hace a las filas), y luego mean
apply(matrizdatos,2,mean)
#analizo que tan similares (o no) son los promedios
#también podemos calcular las varianzas usando la misma función "apply"
apply(matrizdatos,2,var)
#observar diferencias en los resultados de las varianzas


#ahora si corremos la función "prcomp" para obtener las componentes principales, con SCALE=TRUE para
#estandarizar las variables y que todas tengan varianza 1. Al resultado de esto le llamo "pr.out"
pr.out<-prcomp(matrizdatos,scale=TRUE)
#observar cuántas componentes principales hay, y ver si el resultado es esperable (en general hay
#min(n-1,p) componentes principales informativas en una matriz de n individuos y p variables
#según página 417 del Hastie-Tibshirani

#por último generamos el biplot de las dos componentes principales, se pone scale=0 para asegurar
#que las flechas están escaladas para representar las cargas
biplot(pr.out,scale=0)
#los puntos más cercanos al (0,0) son los que tienen valores más parecidas al promedio
#mientras que si observamos las variables y sus ángulos, podemos ver que materias están más o menos
#correlacionadas. A mayor ángulo mayor incorrelación, a menor ángulo más correlación. Ángulo recto
#significa incorrelacionadas.

#si quiero calcular la desviación estandar de cada componente principal
pr.out$sdev
#y la varianza explicada por cada componente principal
pr.var<-pr.out$sdev^2
#para calcular la proporción de la varianza explicada por cada componente principal, dividimos
#la varianza explicada por cada componente sobre la varianza total
pve<-pr.var/sum(pr.var )
#vemos que % de la varianza de los datos explica cada componente principal

#podemos plotear el porcentaje de varianza explicado por cada componente de la siguiente manera
plot(pve,xlab="Componente Principal",ylab="Proporción de la varianza explicada",ylim=c(0,1),type='b')
#y también ploteamos el porcentaje de varianza explicado acumulado
plot(cumsum(pve),xlab="Componente Principal",
     ylab="Proporción acumulada de varianza explicada",ylim=c(0,1),type='b')
