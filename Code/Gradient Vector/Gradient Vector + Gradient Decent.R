library(Deriv)

f <- function(x) x^2 - 10*x + 26
x <- 7
alpha <- 0.01
g.v <- Deriv(f, "x")

for (i in 1:100)
{
  x <- x - alpha * g.v(x)
  print(x)
  print(f(x))
  #Sys.sleep(0.01)
}


f.1 <- function(x1, x2) x1^2 + x2^2 - 2*x1 + x1*x2 + 1
x.1 <- c(5, 5)
alpha.1 <- 0.1
g.v.1 <- Deriv(f.1, c("x1", "x2"))

for (i in 1:100)
{
  x.1 <- x.1 - alpha.1 * attributes(g.v.1(x.1))$gradient
  print(x.1)
  print(f.1(x.1))
}

# (x1 - 4)^2 + x3^2 * x1 + (x2 + 1)^2 + 6
x.2 <- c(5, 5, 5)
alpha <- 0.1

f.2 <- function(x1, x2, x3) (x1 - 4) ^2 + x3^2 * x1 + (x2 + 1)^2 + 6
g.v.2 <- eval(deriv(~(x1 - 4) ^2 + x3^2 * x1 + (x2 + 1)^2 + 6, c("x1","x2","x3"), func=TRUE))

f1.prime <- Deriv(f.2, "x1")
f2.prime <- Deriv(f.2, "x2")
f3.prime <- Deriv(f.2, "x3")
#x1, x2, x3


for (i in 1:1000)
{
  
  #x.2 <- x.2 - (alpha * attributes(g.v.2(x.2[1], x.2[2], x.2[3]))$gradient)
  g.v.2.1 <- c(f1.prime(x.2[1], x.2[2], x.2[3]), f2.prime(x.2[1], x.2[2], x.2[3]), f3.prime(x.2[1], x.2[2], x.2[3]))

  x.2 <- x.2 - (alpha * g.v.2.1)
  x.2 %>% print
}