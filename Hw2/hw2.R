dataset <- Auto

a <- lm(formula = Auto$mpg ~ Auto$cylinders + Auto$displacement + Auto$horsepower + Auto$weight + Auto$acceleration + Auto$year + Auto$origin)
temp <- summary(a)
mean(temp$residuals^2)



Americans <- Auto[Auto$origin == "1",]
ampg <- mean(Americans$mpg)

Japanese <- Auto[Auto$origin == "3",]
jmpg <- mean(Japanese$mpg)

vals <- c()



jmpg - ampg

###################### Q2
Autodummy <- Auto
# for(i in 1:392) {
#   if(Autodummy$origin[i] != 3) { #if its japanese
#     Autodummy$new_origin[i] <- 1
#   }
#   else {
#     Autodummy$new_origin[i] <- 0
#   }
# }
# for(i in 1:392) {
#   if(Autodummy$origin[i] != 2) {
#     Autodummy$new_origin2[i] <- 1
#   }
#   else {
#     Autodummy$new_origin2[i] <- 0
#   }
# }
#gate missing
Auto$origineurope <- ifelse(Auto$origin == 2, 1, 0)
Auto$originamerica <- ifelse(Auto$origin == 1, 1, 0)
model_2a <- lm(mpg ~ origineurope + originamerica, data=Auto)

Auto$new_origin <- ifelse(Auto$origin == 3, 1, 0)
Auto$new_origin2 <- ifelse(Auto$origin == 2, 1, 0)
model_2b <- lm(Auto$mpg ~ Auto$new_origin + Auto$new_origin2, data = Auto)

Auto$new_origin <- ifelse(Auto$origin == 2, 1, -1)
Auto$new_origin2 <- ifelse(Auto$origin == 3, 1, -1)
model_2c <- lm(Auto$mpg ~ Auto$new_origin + Auto$new_origin2, data = Auto)

japanese <- c()
germany <- c()
usa <- c()

for(i in 1:392) {
  if(Auto$origin[i] == "3") {
    Auto$new_origin[i] = "0"
  }
  else if(Autodummy$origin[i] == "1") {
    Auto$new_origin[i] = "1"
  }
  else {
    Auto$new_origin[i] = "2"
  }
}
model_2d <- lm(Auto$mpg ~ Auto$new_origin, data=Auto)


#################################### Q3
c <- lm(formula = Auto$mpg ~ Auto$horsepower + Auto$origin)
summary(c)


###############################Q4

library(gcookbook)

#Get inch to feet
val <- heightweight["heightIn"] / 12
heightweight["heightFt"] <- val


inches <- lm(weightLb ~ heightIn, data=heightweight)
feet <- lm(weightLb ~ heightFt, data=heightweight)
combined <- lm(weightLb ~ heightFt+heightIn, data=heightweight)

mean(inches$residuals^2)
mean(feet$residuals^2)
mean(combined$residuals^2)