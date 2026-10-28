####################################################################################################
## File Name: Openness_Pop_Bivariate_Regression_Example.R
## Date Created: 27 October 2026
## Date Updated: 27 October 2026
## Author: Lowell West
## Assignment/Project: Regression Examples
## Purpose: Showing Bivariate Regresion that is...subideal
####################################################################################################

#### Preamble
## Setting Working Directory

setwd("/Users/lowellawest/Google Drive/Programming/Code_Portfolio/Regression/Openness_Pop")

## Loading Packages
library(countrycode)

#### Putting Data Set together
## GDP (constant 2015 dollars)
gdpdata <- read.csv("World_Bank_GDP.csv")
## Probably in 2015 dollars from codebook
monadtrade <- read.csv("National_COW_4.0.csv")
## Material capabilities
cinc <- read.csv("NMC-70-abridged.csv")

## Getting 2014 data
gdpdataskinny <- gdpdata[c("Country.Code","X2014")]
## Getting COW number
gdpdataskinny$ccode <- countrycode(gdpdataskinny$Country.Code,origin = "iso3c","cown")
## Removing non-COW-state observations
gdpdataskinny <- gdpdataskinny[!is.na(gdpdataskinny$ccode),]
## Final GDP Data Prep
gdpfinal <- gdpdataskinny[,c(3,2)]
names(gdpfinal) <- c("ccode","gdp")
## Getting 2014 data and pop from cinc
cincfinal <- cinc[cinc$year == 2014,c("ccode","tpop")]
## Getting and Totalling Monadic Trade
monadskinny <- monadtrade[monadtrade$year==2014,c("ccode","imports","exports")]
monadskinny$total <- monadskinny$imports + monadskinny$exports
monadfinal <- monadskinny[c("ccode","total")]
## Combining pop and GDP data
transition <- merge(gdpfinal,cincfinal,by="ccode")
## Combining transition with monadic trade
data <- merge(transition,monadfinal,by="ccode")
## Changing total to dollars rather than millions of dollars
data$trade <- data$total*1000000
## Getting openness measure (total trade divided by GDP)
data$openness <- data$trade/data$gdp
## Logging population
data$logpop <- log(data$tpop,base=10)
datanew <- data[!is.na(data$openness),]

#### Analysis
## Linear Model
m1 <- lm(openness~logpop,data=datanew)
summary(m1)

## Plot
# Plot
plot(data$logpop,data$openness,pch=19,col="green",xlab="Population (Thousands) (Logged)",ylab="Trade Openness",main="Openness and Population")
# Regression line
abline(m1,lty=1,col="blue",lwd=2)
# Confidence interval
newx <- seq(min(data$logpop)-1,max(data$logpop)+1,length.out=500)
preds <- predict(m1,newdata = data.frame(logpop=newx),interval = "confidence")
lines(newx,preds[,3],lty=2,col="red")
lines(newx,preds[,2],lty=2,col="red")