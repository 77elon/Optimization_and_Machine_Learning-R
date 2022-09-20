#install.packages("igraph")

# Shortest path
library(igraph)

# Direction Distance Matrix
NW1 <- matrix(c(0,10,20,20,0,0,0,
                0,0,5,15,35,0,0,
                0,0,0,0,35,0,30,
                0,0,0,0,20,15,0,
                0,0,0,0,0,0,15,
                0,0,0,0,10,0,20,
                0,0,0,0,0,0,0),
              byrow=T, ncol=7)
rownames(NW1) <- colnames(NW1) <- c("S",1:5,"T")

# Graph 인접 함수 Weight는 가중치가 들어 있다는 의미...
ig <- graph.adjacency(NW1, mode="directed", weighted=TRUE)
plot(ig, edge.label=E(ig)$weight)

# Distance 인접 함수를 사용하여, 최단 거리를 구함.
# S로 부터 T까지 이동하는 것.
ShortPth <- get.shortest.paths(ig, "S", "T", mode="all", output="both")
ecol <- rep("gray80", ecount(ig))
ecol[unlist(ShortPth$epath)] <- "orange"
ew <- rep(2, ecount(ig))
ew[unlist(ShortPth$epath)] <- 4
vcol <- rep("gray40", vcount(ig))
vcol[unlist(ShortPth$vpath)] <- "gold"
plot(ig, vertex.color=vcol, edge.color=ecol, edge.width=ew, edge.arrow.mode=0)
shortest.paths(ig,"S")

# Minimal spanning tree
library(igraph)
nw.mst <- matrix(c(0,5,3,0,6,5,
                   0,0,5,8,3,7, 
                   0,0,0,7,4,7, 
                   0,0,0,0,6,0, 
                   0,0,0,0,0,8, 
                   0,0,0,0,0,0),
                 byrow=T, ncol=6)
nw.mst <- nw.mst + t(nw.mst)
colnames(nw.mst) <- rownames(nw.mst) <- as.character(1:6)
g <- graph.adjacency(nw.mst, mode="undirected", weighted=TRUE)
plot(g, edge.label=E(g)$weight)
mst.g <- mst(g)
plot(mst.g, edge.label=E(mst.g)$weight)
sum(E(mst.g)$weight) # 길이

# TSP
#install.packages("TSP")
library(TSP)
nw.tsp <- matrix(c(0,5,3,Inf,6,5,
                   0,0,5,8,3,7, 
                   0,0,0,7,4,7, 
                   0,0,0,0,6,Inf,
                   0,0,0,0,0,8,
                   0,0,0,0,0,0),
                 byrow=T, ncol=6)
nw.tsp <- nw.tsp + t(nw.tsp)
colnames(nw.tsp) <- rownames(nw.tsp) <- as.character(1:6)
tsp <- TSP(nw.tsp) 
n_of_cities(tsp) 
labels(tsp)
tour <- solve_TSP(tsp) 
names(tour)
tour # 길이
