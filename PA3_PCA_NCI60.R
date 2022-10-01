#installing package
install.packages("ISLR2")

#loading the package
library(ISLR2)
nci.labs <- NCI60$labs
nci.data <- NCI60$data

#view dimension of data set
dim(nci.data)
#in this case: 64 rows and 6,830 columns. 6,830 gene expression measurements on 64 cancer cell lines

nci.labs[1:4]

#begin by examining the cancer types for the cell lines
table(nci.labs)

# PCA and scaling the variables (genes) to have standard deviation one
pr.out <- prcomp(nci.data , scale = TRUE)

# function that assigns a distinct color to each element of a numeric vector,
# (to each of the 64 cell lines), based on the cancer type to which it corresponds

Cols <- function(vec) {
  cols <-rainbow(length(unique(vec)))
  return(cols[as.numeric(as.factor(vec))])
}
# rainbow() function takes as its argument a positive integer,and returns a vector
#containing that number of distinct colors.

# plot the first few principal component score vectors
par(mfrow = c(1, 2))
plot(pr.out$x[, 1:2], col = Cols(nci.labs), pch = 19,
       xlab = "Z1", ylab = "Z2")
plot(pr.out$x[, c(1, 3)], col = Cols(nci.labs), pch = 19,
       xlab = "Z1", ylab = "Z3")

# cell lines corresponding to a single cancer type do tend to have similar values on the
# first few principal component score vectors.

# summary of the proportion of variance explained (PVE) of the first few principal components
summary(pr.out)$importance[1:3,1:5]

#plot the variance explained by the first few principal components
plot(pr.out)
# the height of each bar in the bar plot is given by squaring the corresponding element of pr.out$sdev.

#more informative to plot the PVE and  cumulative PVE of each principal component
pve <- 100 * pr.out$sdev^2 / sum(pr.out$sdev ^2)
par(mfrow = c(1, 2))
plot(pve , type = "o", ylab = "PVE",
       xlab = "Principal Component", col = "blue")
plot(cumsum(pve), type = "o", ylab = "Cumulative PVE",
       xlab = "Principal Component", col = "brown3")

#there is an elbow in the plot after approximately the seventh principal component. 
#This suggests that there may be little benefit to examining more than seven or so principal
# components (though even examining seven principal components may be difficult)