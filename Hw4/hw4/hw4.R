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

#4a
data4a <- Wage
