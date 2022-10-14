D <- matrix(c(0,458,469,331,411,350,309,349,456,501,262,519,104,496,180,438,153,371,348
,458,0,143,129,48,242,309,214,97,43,296,128,407,178,382,45,313,226,126
,469,143,0,171,155,154,412,136,239,159,239,268,449,321,342,104,350,347,136
,331,129,171,0,84,170,247,141,162,172,187,222,290,236,260,108,192,199,38
,411,48,155,84,0,220,270,192,95,90,262,146,360,178,343,53,265,194,90
,350,242,154,170,220,0,394,28,315,277,93,366,358,396,197,198,267,364,137
,309,309,412,247,270,394,0,549,244,338,367,290,209,235,375,322,185,98,285
,349,214,136,141,192,28,549,0,286,250,103,337,350,367,208,171,257,339,109
,456,97,239,162,95,315,244,286,0,105,348,64,386,84,419,135,303,149,180
,501,43,159,172,90,277,338,250,105,0,336,113,448,176,424,79,354,249,167
,262,296,239,187,262,93,367,103,348,336,0,407,286,422,105,257,208,359,172
,519,128,268,222,146,366,290,337,64,113,407,0,447,73,481,173,366,192,236
,104,407,449,290,360,358,209,350,386,448,286,447,0,413,236,396,99,281,316
,496,178,321,236,178,396,235,367,84,176,422,73,413,0,483,219,343,139,259
,180,382,342,260,343,197,375,208,419,424,105,481,236,483,0,348,192,394,257
,438,45,104,108,53,198,322,171,135,79,257,173,396,219,348,0,299,248,93
,153,313,350,192,265,267,185,257,303,354,208,366,99,343,192,299,0,225,218,
371,226,347,199,194,364,98,339,149,249,359,192,281,139,394,248,225,0,235,
348,126,136,38,90,137,285,109,180,167,172,236,316,259,257,93,218,235,0), ncol=19)

colnames(D) <- c("Artigas", "Canelones", "Colonia", "Durazno", "Florida",
                    "Fray Bentos", "Melo", "Mercedes", "Minas", "Montevideo", "Paysandú", 
                    "Maldonado", "Rivera", "Rocha", "Salto", "San José", "Tacuarembó", "Treinta y Tres", "Trinidad")
rownames(D) <- c("Artigas", "Canelones", "Colonia", "Durazno", "Florida",
                    "Fray Bentos", "Melo", "Mercedes", "Minas", "Montevideo", "Paysandú", 
                    "Maldonado", "Rivera", "Rocha", "Salto", "San José", "Tacuarembó", "Treinta y Tres", "Trinidad")

D

# HACIENDO CALCULO ALGEBRAICO

# Se obtiene la matriz de distancias al cuadrado: D2 = 2(1n1'n − S)
# D2 <- 2 * (rep(1,19)%*%t(rep(1,19))-D) - ESTO SOLO CUANDO SE TIENE MATRIZ DE SIMILITUDES!

D2 <- D^2
D2


# P = In − 1/n 1n1'n, siendo n el conjunto de puntos, en este caso 19.
P <- diag(1,19)-1/19*rep(1,19)%*%t(rep(1,19))
P

# matriz de similaridades Q
Q <- -0.5 * P%*%D2%*%P
Q

# Hay que distinguir si Q es semidefinida positiva (todos los VAP son no 
# negativos) o no (algun VAP es negativo)
eigen(Q)
# Hay valores propios negativos, entonces no es semidefinida positiva, hago transformacion o aproximacion.

# En este caso se hace aproximación
# Ur tiene como columnas los vectores propios asociados a los r valores propios conservados
Ur <- eigen(Q)$vectors[,1:10]
Ur

# Λ es la matriz diagonal que contiene a los valores propios de Q positivos:
Λr <- diag(c(eigen(Q)$values[1:10]))
Λr

Y <- Ur %*% sqrt(Λr)
Y

# se consideran las dos primeras columnas de Y que vienen a ser las dos 
# componentes principales para graficar el biplot

nombres <- c("Artigas", "Canelones", "Colonia", "Durazno", "Florida",
              "Fray Bentos", "Melo", "Mercedes", "Minas", "Montevideo", "Paysandú", 
              "Maldonado", "Rivera", "Rocha", "Salto", "San José", "Tacuarembó", "Treinta y Tres", "Trinidad")
plot(x=Y[,1],y=Y[,2], xlim=c(-200,400),xlab="Y1", ylab="Y2")
text(x=Y[,1],y=Y[,2], xlim=c(-200,400), labels=nombres, cex = 0.6, pos = 4, col = "red")

# ---------------------------------------------------------------------------------------------

# USANDO LA FUNCION CMD
# fit <- cmdscale(d=sqrt(D),eig=TRUE, k=2) si me hubiese dado el cuadrado, uso la funcion sqrt()
fit <- cmdscale(d=sqrt(D2),eig=TRUE, k=2)
fit

# Una medida de la precision conseguida mediante la aproximacion a partir de los VAP
# positivos de la matriz de similitud es el coeficiente dado por el coeficiente de Mardia:
# Si es mayor que 0.8, la aproximacion es buena (coef.Mardia = (VAP utilizados)/sum todos VAPs)

mardia <- (fit$eig[1]+fit$eig[2])/sum(fit$eig[1:10])
mardia

# Como dio 1.03 es buena la aproximacion

# plot solution
x <- fit$points[,1]
y <- fit$points[,2]
plot(x, y, pch=19, xlab="Coordinate 1", ylab="Coordinate 2",
     main="Metric MDS", type="p")
text(x, y, pos=4, labels = row.names(D), cex=.7)
