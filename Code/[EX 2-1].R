library(lpSolve)

obj <- c(8000, 6000)
const <- matrix(c(0.2, 0.4, 0.5, 0.25), ncol = 2, byrow = T)
direction <- c("<=", "<=")
rhs <- c(700, 1000)

lp ("max", obj, const, direction, rhs, int.vec = 1:2)
lp ("max", obj, const, direction, rhs, int.vec = 1:2)$solution
