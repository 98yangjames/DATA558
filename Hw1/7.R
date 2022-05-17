Dataset <- Boston

plot(Boston$medv, Boston$tax)

min <- c()
max <- c()


for(i in 1:13) {
  min <- append(min, min(Boston[i]))
  max <- append(max, max(Boston[i]))
}

vals <- Boston$medv == 50
arr <- c()
pulled <- as.data.frame(c())

for(i in 1:506) {
  if(vals[i] == "TRUE") {
    arr <- append(arr, i)
  }
}
for(i in 1:16) {
  pulled <- rbind(pulled, Boston[i, ])
}

means <- c()
for(i in 1:16) {
  mean <- mean(pulled[, i])
  means <- append(means, mean)
}
data <- Boston[Boston$rm >=8,]