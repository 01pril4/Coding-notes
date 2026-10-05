# Week 3 lecture 
#

# Set data folder as working directory
setwd("~/Downloads/Math2831")


##
## Example: Risk data
##

risk <- read.table('risk.txt',header=T)
head(risk)
names(risk)

risk_mod <- lm(risk~Div+Ratio+Size+Growth+Leverage+Earn1+Earn2, data=risk)
# or
# risk_mod <- lm(risk~., data=risk) ## include all predictors 
summary(risk_mod)

##
## Exercise
##

head(X)

y <- risk$risk 

t(X) # transpose of X
t(X)%*%X # matrix multiplication for X^TX
solve(t(X)%*%X) # inverse of X^TX 
b = solve(t(X)%*%X,t(X)%*%y) #solve normal equations 
b

dim(risk) 
n = dim(risk)[1]
p = dim(risk)[2]

A = (diag(n) - X%*%solve(t(X)%*%X)%*%t(X))
sigma_hat_sq = (t(y)%*%A%*%y)/(n-p)
sigma_hat = sqrt(sigma_hat_sq)

s = round(sigma_hat,digits=4)
print(paste("sigma_hat = ",s))

V = solve(t(X)%*%X)*sigma_hat_sq[1] # Var(b)
se_b = sqrt(diag(V)) #se(b)
data.frame(b,se_b)

x <- 1:20
y <- 2 + 1.5 * x + rnorm(20, 0, 3)
data <- data.frame(x, y)

model <- lm(y ~ x, data = data)
summary(model)

plot(y ~ x, data = data)
abline(model, col = "red")

# ANOVA table 
anova(model) 
anova(model)$`Mean Sq`
F <- anova(model)$`Mean Sq`[1] / anova(model)$`Mean Sq`[2]
F
anova(model)$`F value`
p_value <- 1 - pf(F, df1 = anova(model)$`Df`[1], df = anova(model)$`Df`[2])
p_value
anova(model)$`Pr(>F)`

# Prediction in Simple Linear Regression model 

plot(y ~ x, data = data)
abline(model, col = "red")
abline(2, 1.5, col = "green")

b0 <- model$coefficients[1]
b1 <- model$coefficients[2]

predict(model)
b0 + b1 * data$x

predict(model, data.frame(x=c(12,17)))
b0 + b1 * c(12,17)

newdata=data.frame(x=seq(1,20,length.out=1000))

#95% confidence intervals(innerbands) 
ci=predict(model, newdata, interval="confidence") 
lines(ci[,2] ~ newdata$x, col="red", lty = "dashed") 
lines(ci[,3] ~ newdata$x, col="red", lty = "dashed")  

# 95% predictionintervals(outerbands) 
pi=predict(model, newdata, interval="prediction") 
lines(pi[,2] ~ newdata$x,col="blue", lty= "dotted") 
lines(pi[,3] ~ newdata$x,col="blue", lty= "dotted") 

# Bonferroni adjustment
alpha <- 0.05
k <- 2

confint(model, level= 1 - alpha / k)

confint(model, level = 1 - alpha)
