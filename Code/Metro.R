library(igraph)
metro <- matrix(
              c(0,2,0,0,0,0,0,0,
                2,0,2,0,0,1,0,0,
                0,2,0,2,2,0,0,0,
                0,0,2,0,0,0,0,2,
                0,0,2,0,0,1,3,0,
                0,1,0,0,1,0,2,0,
                0,0,0,0,3,2,0,2,
                0,0,0,2,0,0,2,0),
              byrow=T, ncol=8)
rownames(metro) <- colnames(metro) <- c("S",1:6,"D")

# Graph 인접 함수 Weight는 가중치가 들어 있다는 의미...
ig1 <- graph.adjacency(metro, mode="directed", weighted=TRUE)
plot(ig1, edge.label=E(ig1)$weight)

# Distance 인접 함수를 사용하여, 최단 거리를 구함.
# S로 부터 T까지 이동하는 것.
ShortPth_metro <- get.shortest.paths(ig1, "S", "D", mode="all", output="both")
ecol <- rep("gray80", ecount(ig1))
ecol[unlist(ShortPth_metro$epath)] <- "orange"
ew_1 <- rep(2, ecount(ig1))
ew_1[unlist(ShortPth_metro$epath)] <- 4
vcol <- rep("gray40", vcount(ig1))
vcol[unlist(ShortPth_metro$vpath)] <- "gold"
plot(ig1, vertex.color=vcol, edge.color=ecol, edge.width=ew_1, edge.arrow.mode=0)
shortest.paths(ig1,"S")
