##   
## Week 4 lecture 
##

# Set the working directory
#setwd("~/data")

##
## Example: Risk data
##

risk <- read.table('risk.txt',header=T)

# risk_mod <- lm(risk~Div+Ratio+Size+Growth+Leverage+Earn1+Earn2, data=risk)

risk_mod <- lm(risk~., data=risk) ## include all predictors 
summary(risk_mod)

## confidence intervals for coefficients
confint(risk_mod)

## 95% ci for beta_Div
0.44264 + qt(0.975,17)*0.14170*c(-1,1)


## joint confidence region
#install.packages("ellipse")
library(ellipse)
plot(ellipse(risk_mod,which=c("Earn1","Earn2")),type = 'l')

confint(risk_mod,level=0.975)

## prediction
newdata<-data.frame(Div=0.5, Ratio=1.0, Size= 10.0, Growth=0.1,
                    Leverage=0.3, Earn1=2.0, Earn2=0.6)
pi<-predict.lm(risk_mod,newdata,interval = "prediction"); pi
ci<-predict.lm(risk_mod,newdata,interval = "confidence"); ci

## anova table
anova(risk_mod)


## change the order how predictions enter in the equation
risk_mod2 <- lm(risk~Ratio+Growth+Leverage+Div+Size+Earn2+Earn1,data=risk)
summary(risk_mod2)
anova(risk_mod2)

##
risk_mod_1 <- lm(risk~., data=risk)
risk_mod_0 <- lm(risk ~ 1, data=risk)
anova(risk_mod_0,risk_mod_1)

## compare models
model_null <- lm(risk ~ Div, data=risk)
model_full <- lm(risk ~ Div + Ratio + Size + Growth + Leverage + Earn1 + Earn2, data=risk)
anova(model_null,model_full)


## 
risk_mod_0 <- lm(risk ~ 1, data=risk)
anova(risk_mod_0)
risk_mod_1 <- lm(risk~Div, data=risk)
anova(risk_mod_1)
anova(risk_mod_0,risk_mod_1)

risk_mod_2 <- lm(risk~Div+Ratio, data=risk)
anova(risk_mod_2)
anova(risk_mod_1,risk_mod_2)