2831 Linear Models

# Week 1 lecture 

# getting help 
help(getwd)
?getwd

# get working directory 
getwd() 

# Set data folder as working directory
setwd("~/data")

#
# Example: Market Data 
#

# Load dataset
market <- read.table("Market.txt",header = T) 
market

# Scatterplot

plot(market$Market,market$Host_International) 

plot(Host_International~Market,data=market)   

# Fit linear model

beta<-lm(market$Host_International~market$Market)

beta <- lm(Host_International~Market,data=market)

abline(beta,col="orange") #add fitted line to scatterplot 

# Summary output
summary(beta)
beta$residuals
coef(beta)
beta$coefficients

