library(TSP)

# From csv file import time, name data
nw.tsp  <- read.csv("Optimization/04-14/result_shortest.csv", header=FALSE)
# UTF-8 Encoded 
name <- read.csv("Optimization/04-14/names.csv", header=FALSE)

# data.frame Cols name Deletion
names(name) <- NULL

nw.tsp <- as.matrix(nw.tsp)
#nw.tsp %>% class

# Triangular Matrix + Transposed Triangular Matrix
nw.tsp <- nw.tsp + t(nw.tsp)

colnames(nw.tsp) <- rownames(nw.tsp) <- as.character(name)

tsp <- TSP(nw.tsp) 
n_of_cities(tsp) 
#labels(tsp)
tour <- solve_TSP(tsp) 
tour
tour_route <- names(tour)
idx <- which(tour_route == "학생회관")
cat(tour_route[idx:length(tour_route)], tour_route[1:idx])