state <- as.data.frame(state.x77)
model1 <- lm(Murder ~ Population + Income + Illiteracy + `Life Exp` + Frost, data=state)

model1 %>% summary(.)
model1 %>% print(.)
model1$coefficients %>% sum(.)

y.hat <- model1$fitted.values
y.hat

SSE <- ((state$Murder - y.hat)^ 2) %>% sum #deviance(model1)

y.bar <- mean(state$Murder)
SSR <- ((y.hat - y.bar)^ 2) %>% sum

# sum of squared error (global data)
SSE == deviance(model1) == (residuals((model1))^2 %>% sum(.))
# sum of squared residuals (local data, y.bar)
SSR 

state <- as.data.frame(state.x77)
model1 <- lm(Murder ~ Population + Income + Illiteracy + `Life Exp` + Frost, data=state)

# 벡터, 행렬로 표현
x <- cbind(1, state$Population,  state$Income, state$Illiteracy, state$`Life Exp`, state$Frost) %>% as.matrix(.)
y <- as.matrix(state$Murder) 


cost <- function(x, y, w) {
  sum( (x %*% w - y)^2 ) / (2*length(y))
}

alpha <- 0.00000003
num_iters <- 1000000000

cost_history <- double(num_iters)
w_history <- list(num_iters)

#w <- matrix(c(1.215e+02, 1.700e-04, 4.749e-04, 1.529e+00, -1.658e+00, -1.142e-02), nrow=6)
w <- matrix(rep(0, 6), nrow=6)

# gradient descent
for (i in 1:num_iters) {
  error <- (x %*% w - y)
  delta <- t(x) %*% error
  w <- w - alpha * delta / length(y)
  cost_history[i] <- cost(x, y, w)
  #w_history[[i]] <- w
  #print(c(cost(x, y, w)))
}
which.min(cost_history)
print(c(cost(x, y, w)))
model1$residuals %>% sum()

w %>% print
model1$coefficients