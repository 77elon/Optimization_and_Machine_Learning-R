library(dplyr)
library(primes)
library(lpSolve)

direction <- "max"

objective.in <- c(1:200) %>% matrix(., nrow = 1)

# xi 계수 표현
for (i in 1:200)
  objective.in[i] <- sin(i)^2 + cos(i)


# 1~198에 대한 조건을 저장할 공간 선언
const.1 <- rep(0, 200 * 198) %>% matrix(., ncol = 200)

for(i in 1:198)
{
  # const matrix의 case를 계산한다.
  j <- (i - 1) * 201
  # 계산된 인덱스를 기반으로 조건을 표기한다.
  const.1[j+1] <- const.1[j+2] <- const.1[j+3] <- 1
  
  # 실질적으로 1~198에 대한 입력의 합이 2.758이므로... 저장하는 공간이 추가적으로 필요할 것.
  #const.1[i] <- const.1[i+1] <- const.1[i+2] <- 1
}  

# 2부터 200까지 소수 찾기
prime <- generate_primes(min = 2, max = 200)

# 소수일 경우의 조건 저장 공간 선언
const.2 <- rep(0, 200*length(prime)) %>% matrix(., ncol = 200)

# 소수개 만큼 조건 저장
for(i in 1:length(prime))
{
  j <- (i - 1) * (length(prime)) + prime[i]
  const.2[j] <- 1
}

# 1~200에 대한 조건 저장 공간 선언
const.3 <- rep(0, 200 * 200) %>% matrix(., ncol = 200)

# 1~200의 조건 저장
for(i in 1:200)
{
  j <- (i - 1) * 201
  const.3[j+1] <- 1
}  

# 모든 조건을 하나의 매트릭스로 합친다.
const.mat <- matrix(c(const.1, const.2, const.3), nrow=444, byrow=TRUE)

# 1번 조건 198개, 2번 조건 46개, 3번 조건 200개
const.dir  <- c(rep("<=", 198), rep("<=", length(prime)), rep(">=", 200))

const.rhs <- c(rep(2.758, 198), rep(1.56, 46), rep(0, 200))

model <- lp(direction, objective.in, const.mat, const.dir, const.rhs)

cat("최적해: ", model$objval, "\n") 
cat("x1 ~ 200: ", model$solution, "\n") 

model$solution
