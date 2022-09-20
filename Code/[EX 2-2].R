library(lpSolve)

direction <- "min"
const.in <- c(1000, 3000, 650, 600)
mat <- c(1600, 500, 0, 70000, 10, 0, 0, 140, 120, 0, 0, 0, 7, 14, 13, 16, 0, 1, 0, 0, 0, 0, 1, 0) %>% matrix(ncol = 4, byrow = T)
dir <- c(">=", ">=", ">=", ">=", ">=", ">=")
rhs <- c(5000, 30, 100, 12, 0.1, 0.5)

diet <- lp(direction, const.in, mat, dir, rhs)
diet$solution
