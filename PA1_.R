# EJERCICIO 3
x1 <- c(2, 1, 2, 2)
x2 <- c(3, 5, 2, 3)
x3 <- c(-1, -2, 1, 1)

datos <- cbind(x1, x2, x3)
datos

# PARTE A

#a) El vector de medias (xprom) = (x'1n)/n

#vector de unos
vector.uno <- matrix(rep(1,4),ncol=1)
vector.uno

vector.media <- (t(datos)%*%vector.uno)/4
vector.media

#a) Matriz de covarianza S=1/n(x'*x)-((vector.media)(vector.media)')

matriz.covarianza <- S <- cov(datos)*(4-1)/4
matriz.covarianza

#a) Varianza generalizada VG = det(S)
VG <- det(S)
VG

#a) Matriz de Correlacion R=D^-1/2*S*D^-1/2
DN <- diag(1/sqrt(diag(S)))
DN
matriz.correlacion<-DN%*%S%*%DN
matriz.correlacion

#a) Los Valores y Vectores Propios (de matriz cuadrada de correlacion)

lambda <- eigen(matriz.correlacion)

# Valores propios
lambda$values

# Vectores propios
lambda$vectors

# PARTE B

y1 <- (x1 + x2 + x3)/3
y1

y2 <- (x1 - 0.5*x2 - 0.5*x3)
y2

y <- t(cbind(y1, y2))
y

#b) Matriz de covarianza S=1/n(x'*x)-((vector.media)(vector.media)')

y_matriz.covarianza <- Sy <- cov(y)*(2-1)/2
y_matriz.covarianza

#a) Matriz de Correlacion R=D^-1/2*S*D^-1/2
DNy <- diag(1/sqrt(diag(Sy)))
DNy
y_matriz.correlacion <- DNy%*%Sy%*%DNy
y_matriz.correlacion