library(clv)

c1 = c(1,1,2,2,2,1)
c2 = c(1,2,1,2,1,1)

v.pred <- c1
print(v.pred)
v.real <- c2
print(v.real)

# compare true clustering with those given by the algorithm ( or two outputs of clustering)

# use only once std.ext function
std <- std.ext(v.pred, v.real)

# to compute three indices based on std.ext result

rand1 <- clv.Rand(std)
print(rand1)
jaccard1 <- clv.Jaccard(std)
print(jaccard1)
folk.mal1 <- clv.Folkes.Mallows(std)
print(folk.mal1)

