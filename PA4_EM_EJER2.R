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
# no todos los VAP dieron NO negativos, entonces Q no es semipositivo.

# plot de Q_SM con los componentes principales
nombres <- rownames(D)
plot(x=eigen(Q_SM)$vector[,1],y=eigen(Q_SM)$vector[,2], 
     xlim=c(-1,2),xlab="Y1", ylab="Y2")
text(x=eigen(Q_SM)$vector[,1],y=eigen(Q_SM)$vector[,2], 
     labels=nombres, cex = 0.6, pos = 4, col = "red")

# Pense que para la distancia de Jaccard tenia que hacer la transformacion o aproximacion
# se hace el corrimiento de las distancias que surge al sumar una constante c 
# (que sea mayor o igual que 2|λ| siendo λ el VAP negativo con valor absoluto maximo).

# Al final la transformacion no era necesaria porque el VAP negativo es muuuuy chico, 
# -1.110223e-16, entonces es una aproximacion de 0, no se considera negativo, no hay que hacer aproximacion.
# si se hiciera, seria asi:

# se obtiene la constante c, 2 veces el mayor VAP negativo
c <- -(eigen(Q_JC)$values[3]*2)
c

diag_c <- diag(c, 3)
diag_c

rep_c <- matrix(data = c, nrow = 3, ncol = 3)
rep_c

transformation <- rep_c - diag_c
transformation

# Se hace transformacion sobre la matriz de distancias D2
D2_Jaccard_transf <- D2_Jaccard + transformation
D2_Jaccard_transf

# se obtiene matriz de similaridades Q transformada
Q_JC_transf <- -0.5 * P%*%D2_Jaccard_transf%*%P
Q_JC_transf

eigen(Q_JC_transf)

# se hace la transformacion sobre la matriz de similiaridades, esto NO esta bien! NO hacer!
Q_JC_transf2 <- Q_JC + transformation

eigen(Q_JC_transf2)

