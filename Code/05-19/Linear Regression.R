
data.1 <- data.frame("x" = c(1:10), "y" = c(rep(3, 3), rep(6, 2), rep(9, 3), 10, 11))

plot(data.1$x, data.1$y)
cov(data.1$x, data.1$y)

# 공분산 구하기
x_mean <- mean(data.1$x)
y_mean <- mean(data.1$y)

w1 <- sum((data.1$x - x_mean) * (data.1$y - y_mean)) / sum((data.1$x - x_mean)^2)
w0 <- y_mean - w1 * x_mean

# 결국 정규분포를 따르는 회귀 분석이므로, 
# x가 도출될 수 있는 모든 경우의 수의 평균을 기반해 값을 구한다.
abline(w0, w1, col = "red", lwd = 3)

w1 * 11 + w0

w1 == cov(x, y) / var(x)

model1 <- lm (y ~ x, data=data.1)
model1 %>% summary()

predict(model1, newdata = data.frame('x' = 11))

data.2 <- data.frame(
  'ad_cost' = c(36.5, 28.0, 42.9, 52.0, 51.5, 53.8, 25.4, 37.2, 50.9, 29.2), 
  'new_custom' = c(14, 9, 15, 20, 21, 25, 9, 13, 20, 10))
data.2

plot(data.2$ad_cost, data.2$new_custom)

model2 <- lm(ad_cost ~ new_custom, data=data.2)
model2 %>% summary()
model2 %>% abline(., col='red')
abline(model2, col='red')
model2 %>% fitted.values()

predict.lm(model2, newdata = data.frame('new_custom' = 17))

w0.2 <- model2$coefficients[1]
w0.2
w1.2 <- model2$coefficients[2]
w1.2

(17 - w0) / w1

model2$residuals %>% sum()
