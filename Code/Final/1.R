library(dplyr)
library(lpSolve)

data.1 <- data.frame("X1" = c(5.22, 9.76, 2.13, 1.38, 7.80, 2.59, 3.80, 3.84,3.63, 6.97, 6.80, 6.09, 6.43, 2.44, 1.07, 5.80, 5.93, 7.85, 5.97, 4.27),
                    "X2" = c(2.74, 19.88, 6.16, 12.83, 11.60, 3.79, 17.25, 16.81, 16.39, 4.89, 6.46, 16.19, 3.78, 15.38, 9.48, 5.51, 3.53, 13.76, 10.95, 4.81),
                    "Y" = c(25.43, 52.99, 26.00, 30.52, 41.76, 21.29, 38.91, 38.06, 38.72, 33.56, 32.88, 42.04, 30.06, 36.19, 26.97, 31.06, 27.66, 44.62, 37.22, 27.19))

#선형회귀 모델 생성
model.1 <- lm(Y ~ X1 + (X2) %>% sqrt(.), data = data.1)
#model.1 %>% summary(.)

# 편과 x1, x2의 입력
X <- cbind(1, data.1$X1, (data.1$X2) %>% sqrt(.))
y <- data.1$Y

n <- nrow(data.1)
p <- ncol(data.1)

direction <- "min"

# 0.5, 1 - 0.5

objective.in  <-  c(rep(0.5, n), rep(1-0.5, n), rep(0, 2*ncol(X)))

# x1 * x2^2 = y
#diag(n) %*% -diag(n) %*% X * -X
# diagonal matrix, 입력 행렬의 제곱이 출력과 같아야 한다는 제약 조건
const.mat  <- cbind(diag(n), -diag(n), X, -X)
const.dir <- "="

model.2  <-  lp(direction, objective.in, const.mat, const.dir, y)

result.1 <- model.2$solution
result.2 <- matrix()
for (i in 1:length(result.1)) 
  result.2[i] <- (if(result.1[i]<=0)-1 else +1) * max(result.1[i], result.1[i+p])


# 앞 부분 가중치 제거
w.1  <-  result.2[((2*n)+1):((2*n)+p)]

cat("선형 회귀 가중치: ", model.1$coefficients, '\n')
cat("선형계획법 가중치: ", w.1, '\n')

# y.hat 계산
calc.1 <- matrix(y - X %*% w.1)

cat("선형 회귀 절댓값 오차: ", model.1$residuals %>% abs(.) %>% max(.), '\n')
cat("선형계획법 절댓값 오차: ", calc.1 %>% abs(.) %>% max(.), '\n')

