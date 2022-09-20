library(dplyr)
library(lpSolve)

direction <- "max"
objective.in <- c(20, 50)
const.mat <- matrix(c(1.2, 4,
                   0.5, 1,
                   0, 1,
                   1, 0,
                   0, 1), ncol = 2, byrow = TRUE)

const.dir <- c("<=", "<=", "<=", ">=", ">=") 
const.rhs <- c(240, 81, 40, 0, 0)

cat("최적해: ", lp(direction, objective.in, const.mat, const.dir, const.rhs, all.int = TRUE)$objval, "\n") 
cat("목적함수 x1: ", lp(direction, objective.in, const.mat, const.dir, const.rhs, all.int = TRUE)$solution[1], "\n") 
cat("목적함수 x2: ", lp(direction, objective.in, const.mat, const.dir, const.rhs, all.int = TRUE)$solution[2], "\n") 