############################## 5A

redmean = matrix(c(0,0), nrow = 2, ncol=1)
redsd = diag(2) 
bluemean = matrix(c(1.5, 1.5), nrow = 2, ncol=1)
bluesd = diag(2)
n = 25

x1 <- matrix(rnorm(n, mean=redmean, sd=redsd))
y1 <- matrix(rnorm(n, mean=redmean, sd=redsd))
red <- cbind(x1, y1)

x2 <- matrix(rnorm(n, mean=bluemean, sd=bluesd))
y2 <- matrix(rnorm(n, mean=bluemean, sd=bluesd))
blue <- cbind(x2, y2)


plot(red, col="red", xlim = c(-4, 4), ylim= c(-4,4), xlab = "x1, x2 values", ylab = "y1, y2 values", main = "5a")
points(blue, col="blue")
legend('bottomright', c('red', 'blue'), col = c('red', 'blue'), pch=1)

########################### 5B

# testb <- sample(blue,25)
# testr <- sample(red, 25)
set.seed(10)
testbx <- sample(x2, 25)
testby <- sample(y2, 25)
testb <- cbind(testbx, testby)
testing <- as.data.frame(cbind(testbx, testby))



testrx <- sample(x1, 25)
testry <- sample(y1, 25)
testr <- cbind(testrx, testry)

totalTesting <- rbind(testb, testr)
totalTraining <- rbind(blue, red)

plot(red, col="red", xlab = "x1, x2 values", ylab="y1, y2 values", main = "5b", xlim= c(-4, 4), ylim = c(-4, 4))
points(blue, col="blue")
points(testr, col="red", pch=0)
points(testb, col="blue", pch=0)
legend('bottomright', c( 'Testing Set', 'Training Set', 'Red', 'Blue'), pch = c(0,1, 3, 3), col = c('Black','Black', 'Red', 'Blue'))



########################### 5C
tx1 <- matrix(rnorm(n, mean=redmean, sd=redsd))
ty1 <- matrix(rnorm(n, mean=redmean, sd=redsd))
red <- cbind(x1, y1)

tx2 <- matrix(rnorm(n, mean=bluemean, sd=bluesd))
ty2 <- matrix(rnorm(n, mean=bluemean, sd=bluesd))
blue <- cbind(x2, y2)

tclb <- rep("blue", times = 20)
tclr <- rep("red", times = 20)
tcl <- rbind(clb, clr)

testing_errors <- c()
training_errors <- c()
for (val in 1:20) {
  fit = knn(totalTraining, totalTraining, cl, k=val)
  fit
  cm = as.matrix(table(Actual = cl, Predicted = fit))
  accuracy <- sum(diag(cm)) / length(cl)
  error <- 1 - accuracy
  training_errors <- append(training_errors, error)
  error
}
for (val in 1:20) {
  fit = knn(totalTraining, totalTesting, cl, k=val)
  fit
  cm = as.matrix(table(Actual = tcl, Predicted = fit))
  accuracy <- sum(diag(cm)) / length(cl)
  error <- 1 - accuracy
  testing_errors <- append(testing_errors, error)
  error
}
onek <- c()
onek <- append(onek, 1/1:20)


print(total_errors)
plot(onek, testing_errors, xlab = "1/k", ylab="classification error", ylim = c(0, 0.6), col="blue", main="1/k")
points(onek, training_errors, col = "red")
legend('topright', c('Training', 'Testing'),col = c('red', 'blue'), pch = c(1,1))
############### 5D

misplotted <- as.data.frame(c())
bluemiss <- as.data.frame(c())
redmiss <- as.data.frame(c())
greenmiss <- as.data.frame(c())

for(i in 1:200) {
  if(fit[i] != tcl[i]) {
    misplotted <- rbind(misplotted, testing[i,])
    if(fit[i] == "blue") {
      bluemiss <- rbind(bluemiss, testing[i,])
    }

    if(fit[i] == "red") {
      redmiss <- rbind(redmiss, testing[i,])
    }
    
  }
}

plot(tx1, ty1, col="red", xlab = "x1", ylab = "x2", xlim = c(0,1), main="Distribution of Testing Data (6d)", pch = 0)
points(tx2, ty2, col = "blue", pch = 0)
points(bluemiss[,1], bluemiss[,2], col = "blue", pch = 4)
points(redmiss[,1], redmiss[,2], col = "red", pch = 4)
legend('topright', c('Red Actual', 'Blue Actual', 'Green Actual', 'Color missed'), pch = c(0, 0, 0, 4), col = c('Red', 'Blue', 'Green', 'Black'))



plot(testr, col="red", xlab = "x1, x2 values", ylab="y1, y2 values", main = "5d", xlim= c(-4, 4), ylim = c(-4, 4))
points(testb, col="blue")


legend('bottomright', c( 'Red', 'Blue', 'Mislabeled (color is what it is supposed to be)'), pch = c(3, 3, 4), col = c('Red', 'Blue', 'Red'))


###################### 5E

plot(testr, col="red", xlab = "x1, x2 values", ylab="y1, y2 values", main = "5d", xlim= c(-4, 4), ylim = c(-4, 4))
points(testb, col="blue")



legend('bottomright', c( 'Red', 'Blue', 'Mislabeled (color is what it is supposed to be)'), pch = c(3, 3, 4), col = c('Red', 'Blue', 'Red'))
