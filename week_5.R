##
## Example: Cheddar cheese data
## 

## PRESS statistic for a model with H2S and Lactic
cheese <- read.table("cheese.txt",header = TRUE)
cheese_mod <- lm(taste~H2S+Lactic,data=cheese)

X <- model.matrix(cheese_mod)
H <- (X %*% solve(t(X) %*% X)) %*% t(X)

hat_values <- diag(H)
hat_values - hatvalues(cheese_mod)

pr <- residuals(cheese_mod)/(1-hatvalues(cheese_mod))
PRESS <- sum(pr^2)
PRESS

## 
sum <- summary(cheese_mod)
sum$r.squared
sum$adj.r.squared

## Validation criterion for a model with H2S and Lactic

N <- dim(cheese)[1]
ind <- rep(c(TRUE,FALSE), each= N / 2); ind
ind <- sample(ind); ind
cheese_tr <- cheese[ind,]
cheese_test <- cheese[!ind,] 
cheese_mod_tr <- lm(taste~H2S+Lactic,data=cheese_tr)

newdata = data.frame(cheese_test)
yhat = predict(cheese_mod_tr,newdata)
#yhat=predict(cheese_mod_tr,cheese_test) # this also works 

## test set error sum of squares 
VC=sum((cheese_test$taste-yhat)^2); VC
## test set MSE 
mean((cheese_test$taste-yhat)^2) 

#### alternative way to split into training and test sets
# train = sample(1:30,15)
# test = (-train)
# cheese[train,]
# cheese[test,]
##

## 5-fold CV for a model with H2S and Lactic:
#install.packages("cvTools")
library(cvTools)
cheese_mod <- lm(taste~H2S+Lactic,data=cheese)
#CVC = cvFit(cheese_mod,data=cheese,y=cheese$taste,K=5,seed=1)
CVC = cvFit(cheese_mod,data=cheese,y=cheese$taste,K=5)
CVC$cv

## best subset selection - Mallows' Cp
#install.packages("leaps")
library(leaps)
cp=leaps(x=cheese[,2:4], y=cheese$taste,names=names(cheese)[2:4], method="Cp")
r2=leaps(x=cheese[,2:4], y=cheese$taste, method="r2")
adjr2=leaps(x=cheese[,2:4], y=cheese$taste, method="adjr2")
cbind(cp$which,R2 = r2$r2, AdjR2 = adjr2$adjr2, Cp=cp$Cp)

## best subset selection - using regsubsets
library(leaps)
cheese.best.subsets <- regsubsets(taste~., data=cheese)
summary(cheese.best.subsets)
sum.cheese <- summary(cheese.best.subsets)
names(sum.cheese)
#[1] "which"  "rsq"    "rss"    "adjr2"  "cp"     "bic"    "outmat" "obj"  
sum.cheese$cp
which.min(sum.cheese$cp)
cbind(sum.cheese$which,Rsq_adj=sum.cheese$adjr2,Cp=sum.cheese$cp)

## Mallows' Cp - using ols_mallows_cp(model, fullmodel)
#install.packages("olsrr")
library(olsrr)
cheese <- read.table("cheese.txt",header = TRUE)
cheese_mod <- lm(taste~H2S+Lactic,data=cheese)
cheese_mod_full <- lm(taste~.,data=cheese)
ols_mallows_cp(cheese_mod,cheese_mod_full)

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

#refit lasso model using lambda chosen by cv 
cheese_mod=glmnet(x,y,lambda=path_cv$lambda.min)
coef(cheese_mod)

## 2 ##
hospital=read.table("hospital.txt",header = TRUE)
hospital_mod=lm(y~.,data=hospital)
summary(hospital_mod)

cor(hospital[,1:5])
pairs(hospital[,1:5])

hospital_mod_x1=lm(y~.-x1,data=hospital)
summary(hospital_mod_x1)

output<-summary(lm(x1~x2+x3+x4+x5,data=hospital))
VIF_x1 <- 1/(1-output$r.squared)

output_3<-summary(lm(x3~x1+x2+x4+x5,data=hospital))
VIF_x3 <- 1/(1-output_3$r.squared)

# install.packages("car")
library(car)
vif(hospital_mod)

vif(hospital_mod_x1)
vif(hospital_mod_x1x3)

hospital_mod_x1x3=lm(y~ x2+x4+x5,data=hospital)
summary(hospital_mod_x1x3)

#PRESS Residuals 

i <- 3 

dim(hospital)

  # use this exclude x_3
  #fit model without the i-th data point
  hospital_mod_mi <- lm(y~x1+x2+x3+x4+x5,data=hospital[-i,])
  
  #predict using predictors for i-th data point only
  nd <- hospital[i,]
  
  #use the full dataset to predict
  #find the out of sample prediction error (PRESS residual)
  e_mi <- hospital$y[i] - predict(hospital_mod_mi,newdata = nd)
  e_mi
   #quick way to get the answer
  residuals(hospital_mod)[3]
   
  # USUALLY the first answer is larger than the second one
  
#2
PRESS <- 0

for (i in 1:dim(hospital)[1]){
  hospital_mod_mi <- lm(y~x1+x2+x3+x4+x5,data=hospital[-i,])
  nd <- hospital[i,]
  e_mi <- hospital$y[i] - predict(hospital_mod_mi,newdata = nd)
  PRESS <- PRESS + e_mi^2
}

PRESS