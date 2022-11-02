# Clustering the Observations of the NCI60 Data, Pag. 544
# cluster the cell lines in the NCI60 data, with the goal of finding out whether 
# or not the observations cluster into distinct types of cancer.

library(ISLR2)
nci.labs = NCI60$labs
nci.data = NCI60$data

# standardize the variables to have mean zero and standard deviation one
sd.data = scale(nci.data)

# The data has 64 rows and 6,830 columns.
dim(nci.data)

# hierarchical clustering of the observations using complete, single, 
# and average linkage. Euclidean distance is used as the dissimilarity measure
par(mfrow = c(1, 3))
data.dist = dist(sd.data)
plot(hclust(data.dist), xlab = "", sub = "", ylab = "",
       labels = nci.labs , main = "Complete Linkage")
plot(hclust(data.dist , method = "average"),
       labels = nci.labs , main = "Average Linkage",
       xlab = "", sub = "", ylab = "")
plot(hclust(data.dist , method = "single"),
       labels = nci.labs , main = "Single Linkage",
       xlab = "", sub = "", ylab = "")

# complete and average linkage tend to yield more balanced, attractive clusters.

# We can cut the dendrogram at the height that will yield a particular number of clusters, say 4
hc.out = hclust(dist(sd.data))
hc.clusters = cutree(hc.out , 4)
table(hc.clusters , nci.labs)
# There are some clear patterns. All the leukemia cell lines fall in cluster 3,
# while the breast cancer cell lines are spread out over three different clusters

# plot the cut on the dendrogram that produces these four clusters:
par(mfrow = c(1, 1))
plot(hc.out , labels = nci.labs)
abline(h = 139, col = "red")

hc.out

# Now we do K-means clustering with K = 4
set.seed (2)
km.out = kmeans(sd.data, 4, nstart = 20)
km.clusters = km.out$cluster
table(km.clusters , hc.clusters)

# Rather than performing hierarchical clustering on the entire data matrix, 
# we can simply perform hierarchical clustering on the first few principal 
# component score vectors

# Sometimes performing clustering on the first few principal component score vectors
# can give better results than performing clustering on the full data
