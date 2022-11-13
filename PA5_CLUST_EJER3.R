# a) se aplica k-means fijando los centroides iniciales y se calcula la inercia intra-cluster

# caso 1
x = c(1, 2, 9, 12, 20)
# se definen las posiciones de los centroides
c1 = 1
c2 = 20
# se aplica k-means
cl = kmeans(x, c(c1, c2))
cat("La inercia intra Clusters cuando k = 2 es: ", cl$tot.withinss)
#las asignaciones a los clusters están contenidas en cl$cluster
plot(x, col = cl$cluster, main = "K-Means Clustering Results with K = 2",
     xlab = "", ylab = "")
points(cl$centers, col = 1:2, pch = 8, cex = 2)

# caso 2
c1 = 1
c2 = 12
c3 = 20
cl = kmeans(x, c(c1, c2, c3))
cat("La inercia intra Clusters cuando k = 3 es: ", cl$tot.withinss)
plot(x, col = cl$cluster, main = "K-Means Clustering Results with K = 3",
     xlab = "", ylab = "")
points(cl$centers, col = 1:3, pch = 8, cex = 2)

# caso 3
c1 = 1
c2 = 9
c3 = 12
c4 = 20
cl = kmeans(x, c(c1, c2, c3, c4))
cat("La inercia intra Clusters cuando k = 4 es: ", cl$tot.withinss)
plot(x, col = cl$cluster, main = "K-Means Clustering Results with K = 4",
     xlab = "", ylab = "")
points(cl$centers, col = 1:4, pch = 8)

#La mejor opcion seria el caso 3 que tiene 4 clusters porque la inercia 
# intra-cluster es menor, osea que los grupos son mas homogeneos

# b) Metodo jerarquico ascendente y Dendograma

hc = hclust(dist(x), method = "single")
plot(hc, main = "Single Linkage",
       xlab = "", sub = "", cex = .9)

