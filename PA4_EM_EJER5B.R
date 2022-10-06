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
D2_Sokal_Michener <- as.matrix(2*dist.binary(data, method=2, diag=T,upper=T) ^2)
D2_Sokal_Michener

D2_Jaccard <- as.matrix(2*dist.binary(data, method=1, diag=T,upper=T) ^2)
D2_Jaccard

# P = In − 1/n II', siendo n el conjunto de puntos, en este caso 6.
P <- diag(1,6)-1/6*rep(1,6)%*%t(rep(1,6))
P


# matriz de similaridades Q
Q_SM <- -0.5 * P%*%D2_Sokal_Michener%*%P
Q_SM

Q_JC <- -0.5 * P%*%D2_Jaccard%*%P
Q_JC

# Computes eigenvalues and eigenvectors of complex matrices.
eigen(Q_SM)

eigen(Q_JC)

nombres <- c("leon", "girafa", "vaca","obeja", "gato", "hombre")
plot(x=eigen(Q_SM)$vector[,1],y=eigen(Q_SM)$vector[,2], 
     xlim=c(-0.5,1),xlab="Y1", ylab="Y2")
text(x=eigen(Q_SM)$vector[,1],y=eigen(Q_SM)$vector[,2], 
     labels=nombres, cex = 0.6, pos = 4, col = "red")

