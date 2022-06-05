#Q2
library(factoextra)
nxpMatrix <- matrix(rnorm(36), nrow = 6) #nxp = 6x6
k <- 4
km.res <- kmeans(nxpMatrix, k, nstart = 36)
left <- sum(km.res$withinss) / length(km.res$withinss)

euclidDistance = 0
number_iterations = 0
for(i in 1:36) {
  for(j in 1:24) {
    number_iterations = number_iterations + 1
    euclidDistance = euclidDistance + sqrt(sum((km.res$centers[j] - nxpMatrix[i])^2))
  }
  
}

avg = euclidDistance / number_iterations

#Q3
#a
x <- rbind(matrix(rnorm(1000, mean = 0), nrow = 20),
           matrix(rnorm(1000, mean = 1), nrow = 20), 
           matrix(rnorm(1000, mean = 2), nrow = 20))

#b (dimension reduction)
pca.x <- prcomp(x, scale = TRUE)$x
plot(pca.x[,1:2], col=c(rep("Black",20), rep("Red",20), rep("Blue",20)))

#c
km.x <- kmeans(x, 3, nstart = 25)
table(km.x$cluster, c(rep(1, 20), rep(2,20), rep(3,20)))

#d
km.x2 <- kmeans(x, 2, nstart = 25)
table(km.x2$cluster, c(rep(1, 20), rep(2,20), rep(3,20)))

#e
km.x3 <- kmeans(x, 4, nstart = 25)
table(km.x3$cluster, c(rep(1, 20), rep(2,20), rep(3,20)))

#f
km.x <- kmeans(pca.x[,1:2], 3, nstart = 25)
table(km.x$cluster, c(rep(1, 20), rep(2,20), rep(3,20)))

#g
km.scale <- kmeans(scale(x), 3, nstart = 25)
table(km.scale$cluster, c(rep(1, 20), rep(2,20), rep(3,20)))


#4a
library(ISLR2)
train <- sample(1:nrow(OJ), 800)
model_4a.train <- head(OJ, 800)
model_4a.test <- OJ[-train, ]

#4b
library(e1071)
model_4b <- svm(Purchase ~ ., cost= 0.01, data = model_4a.train, kernel = "linear")
summary(model_4b)

#4c
model_4c <- predict(model_4b, newdata = model_4a.test)
predict_vals <- table(predict = model_4c, truth = model_4a.test$Purchase)
testing_error <- 1 - (sum(diag(predict_vals)) / length(model_4a.test$Purchase))

model_4c <- predict(model_4b, newdata = model_4a.train)
predict_vals <- table(predict = model_4c, truth = model_4a.train)
training_error <- 1 - (sum(diag(predict_vals)) / length(model_4a.train$Purchase))