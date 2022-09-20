library(lpSolve)

obj <- c(75, 20, 40, 50, 40, 25, 15)

obj1 <- c(40, 30, 50, 45)
const1 <- matrix(c(10, 1, 5, 10), c(10, 20, 30, 15), c(10, 5, 20, 15), byrow = T)
direction1 <- c("<=", "<=", "<=")
rhs1 <- c(20, 60, 40)
lp ("max", obj1, const1, direction1, rhs1, binary.vec = 1:4)

const <- matrix(c(5, 2, 1.2, 4, 2.5, 0.5, 0.5), byrow=T)

direction <- "<="

rhs <- c(12)

lp ("max", obj, const, direction, rhs, binary.vec = 1:7)
lp ("max", obj, const, direction, rhs, binary.vec = 1:7)$solution
