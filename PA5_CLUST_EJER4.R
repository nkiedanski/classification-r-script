#install.packages('geosphere')
library('geosphere')

x = rbind( c(1,4), c(1,3), c(0,4), c(5,1), c(6,2), c(4,0))
x

# A) Se grafican los puntos en el plano 

# para configurar el tamano del grafico se define par( mfrow )
par( mfrow= c(1,1) )
plot(x, main = "Observaciones en el plano", xlab = "x", ylab = "y")

# B) Se asignan aleatoriamente una etiqueta de grupo a cada observacion
index = 1:nrow(x)
g1_filas_aleatorias = sample(index, 3)
g1_filas_aleatorias

g2_filas = index[- g1_filas_aleatorias]
g2_filas

# etiquetas del grupo 1
group1 = x[g1_filas_aleatorias,]
group1

# etiquetas del grupo 2
group2 = x[g2_filas,]
group2

# C) se calculan los centroides
centroide_g1 = centroid(group1)
centroide_g1

centroide_g2 = centroid(group2)
centroide_g2

# D) Asignar cada observacion al centroide que este mas cerca
euclidean_dist <- function(x, y) sqrt(sum((x - y)^2))
new_g1 = list()
new_g2 = list()

for (i in index) {
  point = x[i,]
#  print(point)
  dist1 = euclidean_dist(point,centroide_g1)
#  print(dist1)
  dist2 = euclidean_dist(point,centroide_g2)
#  print(dist2)
  if  (dist1 < dist2) {
    new_g1 = rbind(new_g1, point)
  } else {
    new_g2 = rbind(new_g2, point)
  }
}
print(new_g1)
print(new_g2)


