# 1.1 ABalone example 

abalone<-read.table('abalone.txt',header = T)

model<-lm(rings~length,data=abalone)
summary(model)

x <- abalone$length
y <- abalone$rings

model$coefficients
b0 <- model$coefficients[1]
b1 <- model$coefficients[2]

y_hat <- b0 + b1 * x

predict(model)

SStotal <- sum((y - mean(y))^2)
SSreg <- sum((y_hat - mean(y))^2)
SSres <- sum((y_hat - y)^2)

SStotal
SSreg + SSres

R2 <- SSreg / SStotal 
R2_check <- 1 - SSres / SStotal 

plot(rings~length,data=abalone)
abline(model)

plot(predict(model), model$residuals)


# Analysis of Residuals 

plot(model, which=1)
plot(model, which=2)
plot(model, which=3)
plot(model, which=4)
plot(model, which=5)
plot(model, which=6)

# Residual plot analysis 
# Good Residual plots

x <- 1:200 / 5
y <- 1 + 2 * x + rnorm(200, sd = 0.15)
plot(x, y)

model1 <- lm(y ~ x)

abline(model1)

plot(model1, which = 1)
plot(model1, which = 2)

# Bad Residual plots - violation of linearity 

x <- 1:40 / 5
y <- 1 + sin(pi / 2 * x) + rnorm(40, sd = 0.25)
plot(x, y)

model2 <- lm(y ~ x)

abline(model2)

plot(model2, which = 1)

# Bad Residual plots - violation of constant variance  

x <- 1:200
y <- 1 + 2 * x + x * rnorm(200, sd = 0.5)
plot(x, y)

model3 <- lm(y ~ x)

abline(model3)

summary(model3)

plot(model3, which = 1)

# Bad Residual plots - violation of normality

x <- 1:300  
y <- 1 + 2 * x + sample(c(runif(1000, -20, -10), runif(1000, 10, 20)), size = 300)
# At this time its not normal, but has a center

plot(x, y)

model4 <- lm(y ~ x)

abline(model4)

plot(model4, which = 1)
plot(model4, which = 2)
# Nosie is not normal


# 1.2 zinc example
#   
# Week 2 lecture 
#

# Set data folder as working directory
#setwd("~/data")

##
## Example: Zinc data
##

# Load dataset and fit linear model
zinc <- read.csv("zinc.csv")
model_zinc <-lm(plant~sediment,data=zinc)

summary(model_zinc) 

head(zinc)
names(zinc)
dim(zinc)


# how to extract regression coefficient estimates, standard errors, ect. from summary output
sum_mod<- summary(model_zinc)
sum_mod$coefficients
sum_mod$coefficients[,] 
sum_mod$coefficients[,2] #second column
sum_mod$coefficients[2,] #second row

sum_mod$coefficients[2,1] # gives b1
sum_mod$coefficients[2,2] # gives se_b1
# alternatively, 
sum_mod$coefficients["sediment","Estimate"] 
sum_mod$coefficients["sediment","Std. Error"]



# use formulas to find a 95% confidence interval for beta1
b1 = sum_mod$coefficients["sediment","Estimate"] # b1 = 0.8565889
se_b1 = sum_mod$coefficients["sediment","Std. Error"] # se_b1 = 0.1530846
n = dim(zinc)[1]

ci=c(b1-qt(0.975,n-2)*se_b1,b1+qt(0.975,n-2)*se_b1)
ci
round(ci,4)  # round to four decimal places



# Confidence intervals for beta0 and beta1 using confint()
confint(model_zinc) #default 95% ci
confint(model_zinc,level=0.99) #a 99% ci



# Test for a linear relationship, H0: beta1 = 0 vs H1: beta1 != 0
# calculate/extract observed t-test statistic and p-value
t = sum_mod$coefficients["sediment","t value"] #t = 0.8565889/0.1530846
t
pvalue = 2*(1-pt(abs(t),13)); pvalue
sum_mod$coefficients["sediment","Pr(>|t|)"]



# ANOVA output
anova(model_zinc)

# F-test, H0: beta1 = 0 vs H1: beta1 != 0
# calculate/extract observed f-test statistic and p-value
f = anova(model_zinc)["sediment","F value"] #f = 3224.7/103.0
sum_mod$fstatistic 

pvalue = 1-pf(f,1,13); pvalue
anova(model_zinc)["sediment","Pr(>F)"]


##
## Prediction in the simple linear regression model 
##

# predict plant zinc level when sediment zinc level is 70 or 75
predict(model_zinc, data.frame(sediment=c(70,75)))

# confidence interval for mean response
predict(model_zinc, data.frame(sediment=c(70)),interval="confidence",level=0.99)

# prediction interval for a new response
predict(model_zinc, data.frame(sediment=c(70)),interval="prediction",level=0.99)



##
## Simultaneous estimation of regression coefficients
##

# compute at least 95% joint confidence interval for (beta0, beta1) 

confint(model_zinc,level=0.975)

# at least 95% joint ci is (-46.7808106 16.919350)x(0.4688811  1.244297)
