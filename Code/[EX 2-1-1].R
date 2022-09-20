library(lpSolve)

direction <- "max"
objective.in <- c(0.05, 0.2)
const.mat <- matrix(c(1, 1, 0.5, -0.5), byrow = T, ncol = 2)
const.dir <- c("<=", ">=")
const.rhs <- c(10, 0)


portfolio <- lp(direction, objective.in, const.mat, const.dir, const.rhs)
portfolio$solution