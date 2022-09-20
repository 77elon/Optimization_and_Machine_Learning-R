sub1 <- function(x1, x2, x3) x1^4 + 2*x2^2 + 3*x3^2 - 4*x1 - 4*x2*x3  

#x1, x2, x3 편미분과 이차 편미분(deriv x1, x2)을 수행하면, 경사 벡터와 헤시안 행렬을 구할 수 있다.


grad.vec <- c(Deriv(sub, "x1"), Deriv(sub, "x2") ,Deriv(sub, "x3")) %>% matrix() # Col Vector

hessian.mat1 <- c(Deriv(Deriv(sub1, "x1"), "x1"), Deriv(Deriv(sub1, "x1"), "x2"), Deriv(Deriv(sub1, "x1"), "x3"),
                  Deriv(Deriv(sub1, "x2"), "x1"), Deriv(Deriv(sub1, "x2"), "x2"), Deriv(Deriv(sub1, "x2"), "x3"),
                  Deriv(Deriv(sub1, "x3"), "x1"), Deriv(Deriv(sub1, "x3"), "x2"), Deriv(Deriv(sub1, "x3"), "x3")) %>% matrix(., nrow = 3)
hessian.mat1 %>% View()


sub2 <- function(x1, x2) x1^2 + x2^2

grad.vec2 <- c(Deriv(sub2, "x1"), Deriv(sub2, "x2")) %>% matrix(., ncol = 1)

hessian.mat2 <- c(Deriv(Deriv(sub2, "x1"), "x1"), Deriv(Deriv(sub2, "x1"), "x2"), Deriv(Deriv(sub2, "x2"), "x2"), Deriv(Deriv(sub2, "x2"), "x1")) %>% matrix(., nrow = 2)
grad.vec2 %>% View()
hessian.mat2 %>% View()
