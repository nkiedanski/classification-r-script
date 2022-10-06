data <- matrix(c(1,1,1,1,1,0,1,1,0,0,0,0,0,1,0,0,0,0,0,0,1,1,0,0,1,0,0,0,1,
                   1,1,1,1,1,1,0), 
                 ncol=6)

rownames(data) <-c("leon", "girafa", "vaca", "oveja", "gato", "hombre")
colnames(data) <-c("tiene cola", "es salvaje", "tiene cuello largo", 
                   "es animal de granja", "es carnivoro", "camina en 4 patas")
data

# se halla matriz de similaridad 
#con coeficiente de similaridad de Sokal-Michener (cant. variables con resp 1 
#en ambos individuos + cant de variables con resp 0 en ambos individuos)/ cant total variables

# install.packages("clusterSim")
library(clusterSim)

#Matriz de distancia al cuadrado:
D2 <- as.matrix(2* dist.binary(data, method=2, diag=T,upper=T) ^2)
