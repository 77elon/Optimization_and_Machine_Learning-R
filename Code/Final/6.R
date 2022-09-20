library(dplyr)

# 입력 변수 선언.
x <- matrix(c(2,9))
y <- 0.82

# 각 로지스틱, 하이퍼볼릭 탄젠트 함수를 활성 함수로 사용한다.
f.1 <- function(z) 
{
  1/(1+exp(-z))
}
f.2 <- function(z)
{
  tanh(z)
}

# hidden layer weight initialized
a.0 <- x
b.1 <- matrix(c(0.1, 0.5))
b.2 <- matrix(c(3.9))
w.1 <- matrix(c(3.0, 0.5,
                0.7, 2.0), byrow = T, ncol=2)
w.2 <- matrix(c(1.2, -7.0), byrow = T, ncol=2)

df <- data.frame(t(rep(NA,10)))
names(df) <- c("b1.1", "b2.1",
               "w11.1", "w12.1","w21.1","w22.1",
               "b2.1",
               "w11.2","w12.2",
               "y.hat")

for(i in 1:31) {
  # 초기값 기록.
  a.1 <- f.1(b.1 + w.1 %*% a.0)
  a.2 <- as.vector(f.2(b.2 + w.2 %*% a.1))
  y.hat <- a.2
  df[i,] <- c(b.1[1,],b.1[2,],
              w.1[1,1],w.1[1,2],w.1[2,1],w.1[2,2],
              b.2[1,],
              w.2[1,1],w.2[1,2],
              y.hat)
  
  # 학습률은 0.37이다.
  alpha <- 0.37
  
  # 2nd layer output - real value => delta 2 (증명에 의해...)
  delta.2 <- a.2 - y
  
  # tanh를 미분한다면 1 - tanh(x)^2 이므로...
  gr.2 <- delta.2 * (1 - a.2^2) * rbind(1, a.1)
  
  b.2 <- b.2 - alpha * gr.2[1]      # b.2 update
  w.2 <- w.2 - alpha * gr.2[-1]     # w.2 update
  
  # 결국 activation의 통과 값에 대해 편미분을 수행하므로...
  delta.1 <- delta.2 * (1 - a.2^2) * w.2 
  
  # a.1은 로지스틱 함수를 사용하므로, bias를 위해 1에 대한 행렬 곱을 수행한다.
  gr.1 <- (t(delta.1) * (a.1 * (1 - a.1))) %*% cbind(1, t(a.0))
  
  b.1 <- b.1 - alpha * gr.1[,1]     # b.1 update
  w.1 <- w.1 - alpha * gr.1[,-1]     # w.1 update
}
a.1 <- f.1(b.1 + w.1 %*% a.0)
y.hat <- a.2 <- as.vector(f.2(b.2 + w.2 %*% a.1))
df <- rbind.data.frame(df, c(b.1[1,],b.1[2,], 
                             w.1[1,1],w.1[1,2],w.1[2,1],w.1[2,2],
                             b.2[1,],
                             w.2[1,1],w.2[1,2],
                             y.hat))
View(df)
