

### Generate data ###
y <- rnorm(100)
x <- matrix(rnorm(10000*100), ncol = 10000)
error <- rnorm(100, mean = 0, sd = 1)
#####################

#####Predict Model with F(x) = 0 #####
model_1a <- lm(y~x)
zeros <- rep(0, 100)

####Bias Function ####
get_bias = function(estimate, truth) {
  return (mean(estimate) - truth)
  
}
#### Get bias with estimate as all 0s and model predicts ####
#1c(i)
bias <- get_bias(zeros, y) 
bias <- sum(bias)/100

#1c(ii)
variance <- var(y)

#1c (iii) Test MSE = E((y -f(x))^2) 
EPE <- sum((y)^2)/100

# 1c(iv)
testing <- y[80:100]
EPE_validation <- sum(testing^2)/20

#1d
training <- y[0:80]
model_1d <- lm(y[0:80] ~ training)
testing_predicted <- predict(model_1d, newdata = data.frame(testing))
EPE_1d <- sum((testing_predicted - training)^2) / 20

################## Q2

#2a
corr_op1 <- abs(cor(x, y))
hist(corr_op1)

#2b
model_2 <- lm(y[1:10]~training[1:10], drop.unused.levels = TRUE)
model_2option1 <- predict(model_2, newdata = data.frame(training[1:10]))
EPE_2b <- sum((model_2option1 - testing[1:10])^2)/10

#2c 
indexIncreasing <- sort(corr_op1, decreasing = TRUE)
model_2c <- lm(y[1:10] ~ indexIncreasing[1:10], drop.unused.levels = TRUE)
model_2option2 <- predict(model_2c, newdata = data.frame(largest10Q))
EPE_2c <- sum((model_2option2 - testing[1:10])^2)/10



################ Q3

#3b
library(tidyverse)
df_3 <- read.csv("features.csv")
model_3b <- lm(df_3$totalWordsCount ~ ., data = df_3)
x <- df_3 %>% select(4:62)
correlations <- abs(cor(x, df_3$totalWordsCount))
index <- order(correlations, decreasing = TRUE)
largest <- c()
for (i in 1:300) {
  largest <- append(largest, x[index[i]])
}
training3b <- df_3$totalWordsCount[1:700]
testing3b <- df_3$totalWordsCount[700:1000]


model_3bLargest <- lm(training3b ~ ., data=largest, drop.unused.levels = TRUE)
model_3boption2 <- predict(model_3bLargest, newdata = data.frame(largest))
EPE_3b <- sum((model_3boption2[700:1000] - testing3b)^2)/300

#3c
library(glmnet)
lambdas = c(0.01, 1, 10)
ridge3c <- glmnet(data.matrix(x), df_3$totalWordsCount, alpha = 0, lambda = lambdas)
plot(ridge3c, xvar = "lambda", xlab="Lambda")

#3d
ridge3c <- cv.glmnet(data.matrix(x), df_3$totalWordsCount, alpha = 0)
best_lambda <- ridge3c$lambda.min
ridge3d <- glmnet(data.matrix(x), df_3$totalWordsCount, alpha = 0, lambda = 52.307)
model_3d <- predict(ridge3d, newdata = data.frame(training), newx = data.matrix(x))
EPE_3d <- sum((model_3d[700:1000]-testing3b)^2)/300


#3e
lambdas = c(0.01, 1, 3, 5, 10)
lasso3e <- glmnet(data.matrix(x), df_3$totalWordsCount, alpha = 0.9, lambda = 1:10)
plot(lasso3e, xvar = "lambda", xlab="Lambda", ylim = c(0,0.6))

#3f

lasso3f <- cv.glmnet(data.matrix(x), df_3$totalWordsCount, alpha = 1)
best_lambda_3f <- lasso3f$lambda.min
lasso3f <- glmnet(data.matrix(x), df_3$totalWordsCount, alpha = 0.99, lambda = best_lambda_3f)
model_3f <- predict(lasso3f, newdata = data.frame(training), newx = data.matrix(x))
EPE_3f <- sum((model_3f[700:1000] - testing3b)^2)/300


#4a
library(ISLR2)


generate_errors <- function(n) {
  errors <- c()
  for(i in 1:10){
    training_auto <- Auto[1:n,]
    validation_auto <- Auto[(n+1):392,]
    model_4a <- lm(training_auto$mpg ~ poly(horsepower, i), data = training_auto)
    model_4aPredict <- predict(model_4a, newdata = data.frame(validation_auto))
    MSE_4a <- sum((validation_auto$mpg - model_4aPredict)^2)/length(model_4aPredict)
    errors <- append(errors, MSE_4a)
  }
  return (errors)
}

error1 <- generate_errors(100)
error2 <- generate_errors(150)
error3 <- generate_errors(200)
error4 <- generate_errors(250)
error5 <- generate_errors(175)
error6 <- generate_errors(300)


plot(1:10, errors, type = "b", ylab = "MSE", xlab = "polynomial", ylim = c(34, 74))
points(error1, col = "blue", type ="b")
points(error2, col = "red", type ="b")
points(error3, col = "green", type ="b")
points(error4, col = "black", type ="b")
points(error5, col = "purple", type ="b")
points(error6, col = "blue", type ="b")


#4b
lasso_errors <- c()

library(caret)
ctrl <- trainControl(method = "cv")
model_4b <- train(mpg ~ poly(horsepower,1), data = Auto, method = "lm", trControl = ctrl)
lasso_errors <- append(lasso_errors, model_4b$results[2])

model_4b <- train(mpg ~ poly(horsepower,2), data = Auto, method = "lm", trControl = ctrl)
lasso_errors <- append(lasso_errors, model_4b$results[2])

model_4b <- train(mpg ~ poly(horsepower,3), data = Auto, method = "lm", trControl = ctrl)
lasso_errors <- append(lasso_errors, model_4b$results[2])

model_4b <- train(mpg ~ poly(horsepower,4), data = Auto, method = "lm", trControl = ctrl)
lasso_errors <- append(lasso_errors, model_4b$results[2])

model_4b <- train(mpg ~ poly(horsepower,5), data = Auto, method = "lm", trControl = ctrl)
lasso_errors <- append(lasso_errors, model_4b$results[2])

model_4b <- train(mpg ~ poly(horsepower,6), data = Auto, method = "lm", trControl = ctrl)
lasso_errors <- append(lasso_errors, model_4b$results[2])

model_4b <- train(mpg ~ poly(horsepower,7), data = Auto, method = "lm", trControl = ctrl)
lasso_errors <- append(lasso_errors, model_4b$results[2])

model_4b <- train(mpg ~ poly(horsepower,8), data = Auto, method = "lm", trControl = ctrl)
lasso_errors <- append(lasso_errors, model_4b$results[2])

model_4b <- train(mpg ~ poly(horsepower,9), data = Auto, method = "lm", trControl = ctrl)
lasso_errors <- append(lasso_errors, model_4b$results[2])

model_4b <- train(mpg ~ poly(horsepower,10), data = Auto, method = "lm", trControl = ctrl)
lasso_errors <- append(lasso_errors, model_4b$results[2])

plot(1:10, lasso_errors, type = "b", ylab = "MSE", xlab="degree of polynomial", col ="blue")

#4c

generateKfold_errors <- function(n) {
  K_errors <- c()
  ctrl <- trainControl(method = "cv")
  training_auto <- Auto[1:n,]
  validation_auto <- Auto[(n+1):392,]
  model_4c <- train(mpg ~ poly(horsepower,1), data = training_auto, method = "lm", trControl = ctrl)
  K_errors <- append(K_errors, model_4c$results[2])
  
  model_4c <- train(mpg ~ poly(horsepower,2), data = training_auto, method = "lm", trControl = ctrl)
  K_errors <- append(K_errors, model_4c$results[2])
  
  model_4c <- train(mpg ~ poly(horsepower,3), data = training_auto, method = "lm", trControl = ctrl)
  K_errors <- append(K_errors, model_4c$results[2])
  
  model_4c <- train(mpg ~ poly(horsepower,4), data = training_auto, method = "lm", trControl = ctrl)
  K_errors <- append(K_errors, model_4c$results[2])
  
  model_4c <- train(mpg ~ poly(horsepower,5), data = training_auto, method = "lm", trControl = ctrl)
  K_errors <- append(K_errors, model_4c$results[2])
  
  model_4c <- train(mpg ~ poly(horsepower,6), data = training_auto, method = "lm", trControl = ctrl)
  K_errors <- append(K_errors, model_4c$results[2])
  
  model_4c <- train(mpg ~ poly(horsepower,7), data = training_auto, method = "lm", trControl = ctrl)
  K_errors <- append(K_errors, model_4c$results[2])
  
  model_4c <- train(mpg ~ poly(horsepower,8), data = training_auto, method = "lm", trControl = ctrl)
  K_errors <- append(K_errors, model_4c$results[2])
  
  model_4c <- train(mpg ~ poly(horsepower,9), data = training_auto, method = "lm", trControl = ctrl)
  K_errors <- append(K_errors, model_4c$results[2])
  
  model_4c <- train(mpg ~ poly(horsepower,10), data = training_auto, method = "lm", trControl = ctrl)
  lasso_errors <- append(K_errors, model_4c$results[2])
  
  return (lasso_errors)
}

error1 <- generateKfold_errors(100)
error2 <- generateKfold_errors(150)
error3 <- generateKfold_errors(200)
error4 <- generateKfold_errors(250)
error5 <- generateKfold_errors(175)
error6 <- generateKfold_errors(300)



plot(1:10, error2, type = "b", ylab = "MSE * 10", xlab = "polynomial", ylim = c(2.7, 4))
points(1:10, error2, col = "red", type ="b")
points(1:10,error3, col = "green", type ="b")
points(1:10,error4, col = "black", type ="b")
points(1:10,error5, col = "purple", type ="b")
points(1:10,error6, col = "blue", type ="b")


#4d
error_4d <- c()
for(i in 1:10) {
  model_4d <- lm(mpg ~ poly(Auto$horsepower, i), data=Auto)
  predict_4d <- predict(model_4d, newdata = data.frame(Auto$horsepower))
  error_4d <- append(error_4d, sum(model_4d$residuals^2))
}
plot(1:10, error_4d, type = "b", xlab = "Degree of Polynomial", ylab = "Training Set Mean Squared Error")

#####

#4e
i <- 10
model_4e <- lm(mpg ~ poly(Auto$horsepower, i), data=Auto)
summary(model_4e)
