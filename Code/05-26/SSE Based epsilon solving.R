set.seed(1)
x1 <- sample(2:30, 25)
set.seed(2)
x2 <- sample(36:1460, 25)
set.seed(3)
y <- round(2.341 + 1.6159*x1 + 0.015*x2 + rnorm(25,0,3.25),2) 
df1 <- data.frame(x1,x2,y)
View(df1)

model1 <- lm(y ~ x1 + x2, data = df1)
model1$residuals %>% sum


X.1 <- cbind(1, as.matrix(df1[,1:2]))
Y.1 <- df1$y

W.1 <- solve(t(X.1) %*% X.1) %*% t(X.1) %*% Y.1
W.1

# dot product is %*%
Y.1 - (X.1 %*% W.1) %>% sum

model1$coefficients
