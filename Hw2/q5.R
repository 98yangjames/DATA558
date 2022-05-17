n = 100

rnormx <- rnorm(n, mean = 0 , sd = 1)
rnormy <- rnorm(n, mean = 0, sd = 1)

rnormvals <- cbind(rnormx, rnormy)

estimate <- append(estimate, 1/(sqrt(2*3.14))*exp((-0.5*rnormx)^2))