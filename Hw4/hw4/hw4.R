#1
x <- rnorm(30, 0,1)
err <- rnorm(30,0,1)
gx <- exp(x)
y <- gx + err
fun <- function(x){exp(x)}

#1a
plot(x, y)
curve(fun, col = "blue", add = TRUE, lty="dashed")
abline(a=0, b=0, col = 3)
legend("topleft",legend=c("ghat(x)", "generating function"), col= c("green", "blue"), lty=1:2)

#1b
g2 <- sum(y)/30
plot(x, y)
curve(fun, col = "blue", add = TRUE, lty="dashed")
abline(a=g2, b = 0, col = 3)
legend("topleft",legend=c("ghat(x)", "generating function"), col= c("green", "blue"), lty=1:2)

#1c 
vals <- lm(y ~ x)
#coefficents = 2.194379 + 3.105231x
plot(x,y)
curve(fun, col = "blue", add = TRUE, lty="dashed")
abline(a=2.194379, b = 3.105231, col = 3)
legend("topleft",legend=c("ghat(x)", "generating function"), col= c("green", "blue"), lty=1:2)

#1d
xsq = x^2
vals1d <- lm(y ~ x + xsq)
fun1d <- function(x){
  return(1.58870890*x^2 + 2.5476450*x + 0.08390203)
}
plot(x,y)
curve(fun, col = "blue", add = TRUE, lty="dashed")
curve(fun1d, col = "green", add = TRUE, lty = "dashed")
legend("topleft",legend=c("ghat(x)", "generating function"), col= c("green", "blue"), lty=1:2)

#1e
smoothing <- smooth.spline(x,y, all.knots = TRUE, lambda = 0.000001)
fitted <- predict(smoothing, x = seq(min(x) - 0.02, max(x) + 0.02, by = 0.00001))
plot(x,y)
curve(fun, col = "blue", add = TRUE, lty="dashed")
points(smoothing$x, smoothing$y, col = "green", lty = 1, type = "b")
legend("topleft",legend=c("ghat(x)", "generating function"), col= c("green", "blue"), lty=1:2)


#2
yarr = c()

for(x in -2:6) {
  b1x = 0
  b2x = 0
  
  if(x >= 0 & x <= 2) {
    b1x = b1x + 1
    if(x >= 1 & x <= 2) {
      b1x = b1x - (x+1)
    }
  }
  b1x = 3*b1x
  
  if(x >= 3 & x <= 4) {
    b2x = b2x + (2*x-2)
    if(x > 4 & x <= 5) {
      b2x = b2x - 1
    }
  }
  b2x = -2*b2x
  
  y = 2 + b1x + b2x
  yarr = append(yarr, y)
}
plot(-2:6, yarr, xlim = c(-2, 6), ylim = c(-10, 5))


#4
data4a <- Wage
trainingAge <- data4a$age[0:2800]
trainingWage <- data4a$wage[0:2800]
testingAge <- data4a$age[2801:3000]
testingWage <- data4a$wage[2801:3000]

testing <- data.frame(t(rbind(testingAge, testingWage)))

#4a
tasq = trainingAge^2
vals4a <- lm(trainingWage ~ trainingAge + tasq)
#model
fun4a <- function(x) {
  return(-0.05321429*x^2 + 5.31585967*x -10.87610589)
}
plot(data4a$age, data4a$wage)
curve(fun4a, col = "blue", add = TRUE, lty = "dashed", lwd = 3)
legend("topleft",legend=c("fit"), col= c( "blue"), lty=2)

error4a <- sum(vals4a$residuals^2) / length(vals4a$residuals)

#4b
cv.error=rep(0,4)  

for (i in 2:5){
  Wage$tmp <- cut(Wage$age,i)
  fit.step = glm(wage~tmp, data = Wage)
  cv.error[i] <- cv.glm(Wage ,fit.step, K= 10)$delta
}
BestSE <- min(cv.error[2:4])
BestCut <- which(cv.error==BestSE)
agelims =range(Wage$age)
age.grid=seq(from=agelims [1],to=agelims [2])
fit.step=lm(wage~cut(age ,BestCut),data=Wage)
preds=predict(fit.step ,newdata =list(age=age.grid),se=TRUE)
plot(data4a$age, data4a$wage)
lines(age.grid ,preds$fit,lwd=3,col="blue", lty = "dashed")

error4b <- sum(fit.step$residuals^2)/ length(fit.step$residuals)

#4c
library(ggplot2)
test <- sample(1:nrow(Wage), 200)
wage_train <- Wage[-test, ]
wage_test <- Wage[test,]
agelims =range(Wage$age)
age.grid=seq(from=agelims [1],to=agelims [2])
cuts <- levels(cut(wage_train$age,8))
model_1 <- lm(wage~poly(age, 4), data=wage_train[wage_train$age <= 25.8,])
model_2 <- lm(wage~poly(age, 4), data=wage_train[wage_train$age > 25.8 & wage_train$age <= 33.5,])
model_3 <- lm(wage~poly(age, 4), data=wage_train[wage_train$age > 33.5 & wage_train$age <= 41.2,])
model_4 <- lm(wage~poly(age, 4), data=wage_train[wage_train$age > 41.2 & wage_train$age <= 49,])
model_5 <- lm(wage~poly(age, 4), data=wage_train[wage_train$age > 49 & wage_train$age <= 56.8,])
model_6 <- lm(wage~poly(age, 4), data=wage_train[wage_train$age > 56.8 & wage_train$age <= 64.5,])
model_7 <- lm(wage~poly(age, 4), data=wage_train[wage_train$age > 64.5 & wage_train$age <= 72.2,])
model_8 <- lm(wage~poly(age, 4), data=wage_train[wage_train$age > 72.2,])
model_1Pred <- predict(model_1, newdata = wage_test)
plot(Wage$age, Wage$wage)
prediction_1 <- predict(model_1, newdata=wage_test[wage_test$age <= 25.8,])
prediction_2 <- predict(model_2, newdata=wage_test[wage_test$age > 25.8 & wage_test$age <= 33.5,])
prediction_3 <- predict(model_3, newdata=wage_test[wage_test$age > 33.5 & wage_test$age <= 41.2,])
prediction_4 <- predict(model_4, newdata=wage_test[wage_test$age > 41.2 & wage_test$age <= 49,])
prediction_5 <- predict(model_5, newdata=wage_test[wage_test$age > 49 & wage_test$age <= 56.8,])
prediction_6 <- predict(model_6, newdata=wage_test[wage_test$age > 56.8 & wage_test$age <= 64.5,])
prediction_7 <- predict(model_7, newdata=wage_test[wage_test$age > 64.5 & wage_test$age <= 72.2,])
prediction_8 <- predict(model_8, newdata=wage_test[wage_test$age > 72.2,])
plot_4 <- ggplot(wage_test, aes(x=age, y=wage)) + geom_point(alpha=0.5) + theme(panel.background = element_rect(fill = "white", colour = "white"))
plot_4 + 
  geom_line(data=wage_test[wage_test$age <= 25.8,], aes(x=age, y=prediction_1), size=1, col="blue") + 
  geom_line(data=wage_test[wage_test$age > 25.8 & wage_test$age <= 33.5,], aes(x=age, y=prediction_2), size=1, col="blue")+ 
  geom_line(data=wage_test[wage_test$age > 33.5 & wage_test$age <= 41.2,], aes(x=age, y=prediction_3), size=1, col="blue")+ 
  geom_line(data=wage_test[wage_test$age > 41.2 & wage_test$age <= 49,], aes(x=age, y=prediction_4), size=1, col="blue")+ 
  geom_line(data=wage_test[wage_test$age > 49 & wage_test$age <= 56.8,], aes(x=age, y=prediction_5), size=1, col="blue")+ 
  geom_line(data=wage_test[wage_test$age > 56.8 & wage_test$age <= 64.5,], aes(x=age, y=prediction_6), size=1, col="blue")+ 
  geom_line(data=wage_test[wage_test$age > 64.5 & wage_test$age <= 72.2,], aes(x=age, y=prediction_7), size=1, col="blue")+ 
  geom_line(data=wage_test[wage_test$age > 72.2,], aes(x=age, y=prediction_8), size=1, col="blue")
  


#4d
agelims =range(data4a$age)
age.grid<-seq(from=agelims[1], to = agelims[2])
fit4d<-lm(wage ~ bs(age,knots = c(25,40,60)),data = data4a )

plot(data4a$age,data4a$wage)
points(age.grid,predict(fit4d,newdata = list(age=age.grid)),col="blue",lwd=2,type="l")
abline(v=c(25,40,60),lty=2,col="blue")

error4d <- sum(fit4d$residuals^2)/length(fit4d$residuals)

#4e
agelims =range(data4a$age)
age.grid<-seq(from=agelims[1], to = agelims[2])
fit4e<-smooth.spline(data4a$age, data4a$wage, nknots = 5)

plot(data4a$age,data4a$wage)
lines(age.grid[1:61], fit4e$y, col = "blue", lwd = 3)

#5a
library(tree)
library(ISLR2)
library(dplyr)
model_5a <- select(Auto, -name)
High <- factor(ifelse(Auto$mpg <= 23.5, "No", "Yes"))
model_5a <- data.frame(model_5a, High)

train <- sample(1:nrow(Auto), 300)
model_5a.train <- head(model_5a, 300)
model_5a.test <- model_5a[-train, ]
High.test <- High[-train]

tree.model_5a <- tree(High ~ . - mpg, model_5a, subset = train)
tree.pred <- predict(tree.model_5a, model_5a.test, type = "class")
plot(tree.model_5a)
text(tree.model_5a, pretty = 0)
acctab <- table(tree.pred, High.test)
accuracy <- sum(diag(acctab)) / sum(acctab)


cv.model_5a <- cv.tree(tree.model_5a, FUN = prune.misclass)
par(mfrow = c(1, 2))
plot(cv.model_5a$size, cv.model_5a$dev, type = "b")
plot(cv.model_5a$k, cv.model_5a$dev, type = "b")
prune.model_5a <- prune.misclass(tree.model_5a, best = 4)

plot(prune.model_5a)
text(tree.model_5a)
tree.pred <- predict(prune.model_5a, model_5a.test, type = "class")
acctab2 <- table(tree.pred, High.test)
accuracy2 <- sum(diag(acctab2)) / sum(acctab2)
yhat <- predict(tree.model_5a, newdata = model_5a.test)
MSE5a <- mean((yhat - model_5a.test$mpg)^2)

#5b
library(ipred) 
bag <- bagging(formula = model_5a.train$mpg ~ ., data = model_5a.train, nbag = 150, coob = TRUE)
bag.model_5b <- predict(bag, newdata = model_5a.test)
MSE5b <- mean((bag.model_5b - model_5a.test$mpg)^2)
#5c
library(randomForest)
train <- sample(1:nrow(Auto), 300)
model_5a.test <- model_5a[-train, ]
model_5a.test <- model_5a.test[-c(2, 8,9) ]
set.seed(1)
bag.model_5a <- randomForest(mpg ~ displacement + horsepower + weight + acceleration + year, data = Auto, subset = train, importance = TRUE)

plot(bag.model_5a)
yhat.bag <- predict(bag.model_5a, newdata = model_5a.test)
MSE <- mean((yhat.bag - model_5a.test$mpg)^2)


#5d
model_5a.train <- head(Auto, 300)
model_5a.test <- model_5a[-train, ]
model_5a.test <- model_5a.test[-c(2, 8,9) ]
library(gam)
gam.model_5a <- gam(mpg ~ s(displacement,4) + s(horsepower, 4) + s(weight,4) + s(acceleration,4) + s(year,4), data = model_5a.train)
plot.Gam(gam.model_5a)
gam.predict <- predict(gam.model_5a, newdata =model_5a.test)
MSE5d <- mean((gam.predict - model_5a.test$mpg)^2)