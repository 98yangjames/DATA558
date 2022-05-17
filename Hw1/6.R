#INITIALIZERS

n = 200
x1 <- runif(n, 0, 1)
x2 <- runif(n, 0, 1)

cl <- c()

redx <- c()
redy <- c()

greenx <- c()
greeny <- c()

bluex <- c()
bluey <- c()

for(i in 1:200) {
  val <- (x1[i] - 0.5)^2 + (x2[i] - 0.5)^2
  if(val > 0.15 && x1[i] > 0.5) {
    print(i)
    cl <- append(cl, "red")
    redx <- append(redx, x1[i])
    redy <- append(redy, x2[i])
  }
  else if(val > 0.15 && x1[i] <= 0.5){
    
    cl <- append(cl, "green")
    greenx <- append(greenx, x1[i])
    greeny <- append(greeny, x2[i])
  }
  else {
    cl <- append(cl, "blue")
    bluex <- append(bluex, x1[i])
    bluey <- append(bluey, x2[i])
  }
}
print(cl)

plot(redx, redy, col="red", xlab = "x1", ylab = "x2", xlim = c(0,1), main="Distribution of Training Data (6a)", pch = 1)
points(bluex, bluey, col ="blue", pch = 1)
points(greenx, greeny, col="green", pch = 1)
legend("topright", c('Testing Set', 'Training Set'), pch = c(0, 1), col = c('Black', 'Black'))


############### 6b
n = 200
tx1 <- runif(n, 0, 1)
tx2 <- runif(n, 0, 1)
i <- 0

tcl <- c()

tredx <- c()
tredy <- c()

tgreenx <- c()
tgreeny <- c()

tbluex <- c()
tbluey <- c()

for(i in 1:200) {
  val <- (tx1[i] - 0.5)^2 + (tx2[i] - 0.5)^2
  if(val > 0.15 && tx1[i] > 0.5) {
    
    tcl <- append(tcl, "red")
    tredx <- append(tredx, tx1[i])
    tredy <- append(tredy, tx2[i])
  }
  else if(val > 0.15 && tx1[i] <= 0.5){
    
    tcl <- append(tcl, "green")
    tgreenx <- append(tgreenx, tx1[i])
    tgreeny <- append(tgreeny, tx2[i])
  }
  else {
    tcl <- append(tcl, "blue")
    tbluex <- append(tbluex, tx1[i])
    tbluey <- append(tbluey, tx2[i])
  }
}


plot(redx, redy, col="red", xlab = "x1", ylab = "x2", xlim = c(0,1), main="Distribution of Training Data (6b)", pch = 1)
points(bluex, bluey, col ="blue", pch = 1)
points(greenx, greeny, col="green", pch = 1)
points(tredx, tredy, col = "red", pch = 0)
points(tbluex, tbluey, col = "blue", pch = 0)
points(tgreenx, tgreeny, col = "green", pch = 0)

legend("topright", c('Testing Set', 'Training Set'), pch = c(0, 1), col = c('Black', 'Black'))

############################### 6c
testing_errors <- c()
training_errors <- c()

training <- as.data.frame(cbind(x1, x2))
testing <- as.data.frame(cbind(tx1, tx2))


for (k in 1:50) {
  
  fit = knn(training, training, cl, k=k)
  cm = as.matrix(table(Actual = cl, Predicted = fit))
  accuracy <- sum(diag(cm))/length(cl)
  error <- 1 - accuracy
  training_errors <- append(training_errors, error)
}
print(training_errors)



for (k in 1:50) {
  
  fit = knn(training, testing, cl, k=k)
  cm = as.matrix(table(Actual = tcl, Predicted = fit))
  accuracy <- sum(diag(cm))/(length(cl))
  error <- 1 - accuracy
  testing_errors <- append(testing_errors, error)
}
print(testing_errors)

plot(1/1:50, training_errors, col="red", ylim = c(0,0.8), xlim= c(0,1), main = "6c", ylab= "training and testing errors", xlab="1/k")
points(1/1:50, testing_errors, col="blue")
legend('topright', c('Testing', 'Training'), pch = c(1, 1), col = c('Blue', 'Red'))

############################## 6d
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
    if(fit[i] == "green") {
      greenmiss <- rbind(bluemiss, testing[i,])
    }
    if(fit[i] == "red") {
      redmiss <- rbind(redmiss, testing[i,])
    }

  }
}

plot(tredx, tredy, col="red", xlab = "x1", ylab = "x2", xlim = c(0,1), main="Distribution of Testing Data (6d)", pch = 0)
# points(bluex, bluey, col ="blue", pch = 1)
# points(greenx, greeny, col="green", pch = 1)
# points(tredx, tredy, col = "red", pch = 0)
points(tbluex, tbluey, col = "blue", pch = 0)
points(tgreenx, tgreeny, col = "green", pch = 0)
# points(misplotted[,1], misplotted[,2], col = "black", pch = 4)
points(bluemiss[,1], bluemiss[,2], col = "blue", pch = 4)
points(redmiss[,1], redmiss[,2], col = "red", pch = 4)
points(greenmiss[,1], greenmiss[,2], col = "green", pch = 4)
legend('topright', c('Red Actual', 'Blue Actual', 'Green Actual', 'Color missed'), pch = c(0, 0, 0, 4), col = c('Red', 'Blue', 'Green', 'Black'))



