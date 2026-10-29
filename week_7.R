##   
## Week 7 Lecture 1 
##

# Set the working directory
setwd("~/data")

##
## Example: Cheddar cheese data
## 

cheese <- read.table("cheese.txt",header = TRUE)

#Forward Selection

#Step 0

cheese_mod_0 <- lm(taste~1, data=cheese)
extractAIC(cheese_mod_0)[2]

#Step 1


#Step 2


#Step 3



## forward, backward and stepwise selection
#install.packages("MASS")
library(MASS)
cheese_mod <- lm(taste~1, data=cheese)
step <- stepAIC(cheese_mod, scope=~Acetic+H2S+Lactic, direction="forward")
step$coefficients

cheese_mod <- lm(taste~Acetic+H2S+Lactic,data=cheese)
step <- stepAIC(cheese_mod, direction="backward")
step$coefficients

cheese_mod <- lm(taste~Acetic,data=cheese)
step <- stepAIC(cheese_mod,scope=~Acetic+H2S+Lactic, direction="both")
step$coefficients

## LASSO 
#install.packages("glmnet") 
library(glmnet) 
#set random seed #set.seed(1)
cheese <- read.table("cheese.txt",header = TRUE) 
x=as.matrix(cheese[,-1]) 
y=cheese$taste 

#fit lasso model (default) 
lasso.mod = glmnet(x, y) 
#plot coefficients of the fitted model (log lambda on x-axis) 
plot(lasso.mod,xvar="lambda") 
#cross validation path 
path_cv=cv.glmnet(x,y) 
plot(path_cv)	

plot(lasso.mod,xvar="lambda") 
abline(v = log(path_cv$lambda.1se))
abline(v = log(path_cv$lambda.min))

#refit lasso model using lambda chosen by cv 
cheese_mod=glmnet(x,y,lambda=path_cv$lambda.min)
coef(cheese_mod)


## Week 7 Lecture 2 
##
## Example: inflation data
##

inflation <- read.table('inflation.txt',header = T)
inf_mod <- lm(INF~QUES+LEGAL+DEV,data=inflation)
par(mfrow=c(1,2)) #two plots side by side
plot(inf_mod,which=c(1,2))
par(mfrow=c(1,1))

##
## Example: risk data
##

risk <- read.table("risk.txt",header = TRUE)
risk_mod <- lm(risk~Div+Ratio+Size+Growth+Leverage+Earn1+Earn2,data=risk)
par(mfrow=c(1,2)) 
plot(risk_mod,c(1,2))
par(mfrow=c(1,1))


##
## Example: capital asset pricing data
##

capital <- read.table('capmodel.txt',header=T)
capital <- capital[-c(20, 19), ] #two points removed
cap_mod <- lm(Price~PE_ratio+TSE300,data=capital)
plot(Price~TSE300,data=capital)

par(mfrow=c(1,2))
plot(cap_mod,c(1,2))
par(mfrow=c(1,1))

plot(Price~TSE300,data=capital,ylim=c(8,13))
abline(lm(Price~TSE300,data=capital))
capital_sub <- capital[-c(19),] #influential point (obs.21) removed 
abline(lm(Price~TSE300,data=capital_sub),col="red") #refit model 

plot(cap_mod,3) #Scale-Location plot


residuals(cap_mod)[19] #residual of obs.21 is e_21 = -0.3577337
r_21=rstandard(cap_mod)[19] #internally studentized residual of obs.21 is r_21 = -2.570001
sqrt(abs(r_21)) #1.603122

## testing for outliers 
cap_mod_1 <- lm(Price~TSE300,data=capital)
t_21 <- rstudent(cap_mod_1)[19] #-3.370171
pvalue = 2*pt(t_21,16)
pvalue

##
## added variables and partial residuals plots
##

summary(cap_mod)

## residual plots 
par(mfrow=c(2,2))
plot(cap_mod)
par(mfrow=c(1,1))

## added variables and partial residuals plots
library(car)
avPlots(cap_mod)
crPlots(cap_mod)