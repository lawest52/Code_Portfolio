####################################################################################################
## File Name: Idealized_Regression.R
## Date Created: 10 October 2026
## Date Updated: 10 October 2026
## Author: Lowell West
## Assignment/Project: Regression Examples
## Purpose: Show the idealized version of regression
####################################################################################################

#### Preamble
set.seed(1885)


#### Bivariate regression
## Randomly generated independent variable, normal, mean 0, sd 1
x1 <- rnorm(100)
## Dependent variable with mean 2.5*x1 and sd 1
y1 <- 1.5*x1 + rnorm(100)
## Regression
m1 <- lm(y1~x1)
summary(m1)
## Plotting
# Plot
plot(x1,y1,pch=19,col="green",xlab="",ylab="")
# Regression line
abline(m1,lty=1,col="blue",lwd=2)
# Confidence interval
newx = seq(min(x1)-1,max(x1)+1,length.out=500)
conf_interval <- predict(m1, newdata=data.frame(x1=newx),interval='confidence',level=0.95)
lines(newx, conf_interval[,2], col="blue",lty=2)
lines(newx, conf_interval[,3], col="blue",lty=2)