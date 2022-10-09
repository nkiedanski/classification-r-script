D <- matrix(c(1, 1, 0, 0, 1, 1,
              1, 1, 1, 0, 0, 1, 
              1, 0, 0, 1, 0, 1), ncol = 6)

colnames(D) <- c("x1", "x2", "x3", "x4", "x5", "x6")
rownames(D) <- c("x1", "x2", "x3")

D

# install.packages("clusterSim")
library(clusterSim)

#Matriz de distancia al cuadrado:
D2_Sokal_Michener <- as.matrix(2*dist.binary(D, method=2, diag=T,upper=T) ^2)
D2_Sokal_Michener

D2_Jaccard <- as.matrix(2*dist.binary(D, method=1, diag=T,upper=T) ^2)
D2_Jaccard

# P = In − 1/n II', siendo n el conjunto de puntos, en este caso 3.
P <- diag(1,3)-1/3*rep(1,3)%*%t(rep(1,3))
P


# matriz de similaridades Q
Q_SM <- -0.5 * P%*%D2_Sokal_Michener%*%P
Q_SM

Q_JC <- -0.5 * P%*%D2_Jaccard%*%P
Q_JC

# Computes eigenvalues and eigenvectors of complex matrices.
eigen(Q_SM)
# todos los valores propios dieron no negativos, entonces Q es semipositiva

eigen(Q_JC)
# no todos los VAP dieron no negativos, entonces Q no es semipositivo

nombres <- rownames(D)
plot(x=eigen(Q_SM)$vector[,1],y=eigen(Q_SM)$vector[,2], 
     xlim=c(-1,2),xlab="Y1", ylab="Y2")
text(x=eigen(Q_SM)$vector[,1],y=eigen(Q_SM)$vector[,2], 
     labels=nombres, cex = 0.6, pos = 4, col = "red")
