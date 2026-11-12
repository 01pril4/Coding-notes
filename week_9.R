##   
## Weeks 9 

##
## Example: fish biomass data
##

biodata <- read.table('biomass.txt',header = TRUE)
model <- lm(y~x4,data=biodata)
summary(model)
influence.measures(model)

#remove influential observations and refit 
model2 <- lm(y~x4,data=biodata[-c(3,4),])
summary(model2)

#effect on confidence and prediction intervals
newdata <- data.frame(x4=c(50))
predict(model, newdata, interval="confidence")
predict(model, newdata, interval="prediction")

predict(model2, newdata, interval="confidence")
predict(model2, newdata, interval="prediction")

##
## Example: snow geese data
##

geese <- read.csv("geese.csv")
geese_mod <- lm(obs1~photo,data=geese)

par(mfrow=c(1,2))
plot(obs1~photo,data=geese) # scatterplot
abline(geese_mod)
plot(geese_mod,1)	
par(mfrow=c(1,1))

# diagnostic plots 
par(mfrow=c(2,2))
plot(geese_mod)	
par(mfrow=c(1,1))

##
## square root tranformation of response
##
geese$obs_tr <- sqrt(geese$obs1)
geese_mod_tr <- lm(obs_tr~photo,data=geese)

par(mfrow=c(1,2))
plot(obs_tr~photo,data=geese)
abline(geese_mod_tr)
plot(geese_mod_tr,1)
par(mfrow=c(1,1))

# diagnostic plots
par(mfrow=c(2,2))
plot(geese_mod_tr)	
par(mfrow=c(1,1))

##
## square root tranformation of both response and predictor
##
geese$photo_tr <- sqrt(geese$photo) #not just predictor, but also the response
geese_mod_tr <- lm(obs_tr~photo_tr,data=geese)

par(mfrow=c(1,2))
plot(obs_tr~photo_tr,data=geese)
abline(geese_mod_tr)
plot(geese_mod_tr,1)
par(mfrow=c(1,1))

# diagnostic plots
par(mfrow=c(2,2))
plot(geese_mod_tr)	
par(mfrow=c(1,1))

##
## log-log  tranformation
##
geese$obs_log <- log(geese$obs1)
geese$photo_log <- log(geese$photo)
geese_mod_log <- lm(obs_log~photo_log,data=geese)

par(mfrow=c(1,2))
plot(obs_log~photo_log,data=geese)
abline(geese_mod_log)
plot(geese_mod_log,1)
par(mfrow=c(1,1))

# diagnostic plots
par(mfrow=c(2,2))
plot(geese_mod_log)	
par(mfrow=c(1,1))


##
## Example (log transformation): inflation data 
##
inflation <- read.table("inflation.txt",header = T)

# fit model to untransformed data
inf_mod=lm(INF~QUES,data=inflation)
par(mfrow=c(2,2))
plot(inf_mod)
par(mfrow=c(1,1))

# log transform of response
par(mfrow=c(1,2)) 
plot(INF~QUES, data=inflation)
plot(log(INF)~QUES, data=inflation)
par(mfrow=c(1,1)) 

# fit model to log transformed data
inf_log_mod=lm(log(INF)~QUES,data=inflation)
par(mfrow=c(2,2))
plot(inf_log_mod)
par(mfrow=c(1,1))

# remove observation 22 and refit
inf_mod_22<-lm(log(INF)~QUES,data = inflation[-c(22),] )
par(mfrow=c(2,2)) 
plot(inf_mod_22)
par(mfrow=c(1,1)) 

##
## comparing PRESS statistics 
##
inf_mod <- lm(INF~QUES,data=inflation)
inf_mod_22_20 <- lm(INF~QUES,data = inflation[-c(20,22),] )

inf_log_mod <- lm(log(INF)~QUES,data=inflation)
inf_log_mod_22_20 <- lm(log(INF)~QUES,data=inflation[-c(20,22),])

PRESS <- function(linear.model) {
  pr <- residuals(linear.model)/(1 - hatvalues(linear.model))
  sum(pr^2)
}
PRESS(inf_mod)
PRESS(inf_mod_22_20) #u cannot compare these two

inflation$INF    
inflation[-c(20,22),]$INF

residuals(inf_log_mod)/(1 - hatvalues(inf_log_mod))
sum((inflation$INF-exp(log(inflation$INF)-residuals(inf_log_mod)/(1 - hatvalues(inf_log_mod))))^2)->PRESS_log_inf
PRESS_log_inf #this result can be compared with PRESS(inf_mod)
 
residuals(inf_log_mod_22_20)/(1 - hatvalues(inf_log_mod_22_20))
sum((inflation[-c(20,22),]$INF-exp(log(inflation[-c(20,22),]$INF)-residuals(inf_log_mod_22_20)/(1 - hatvalues(inf_log_mod_22_20))))^2)->PRESS_log_inf_22_20
PRESS_log_inf_22_20 #this result can be compared with PRESS(inf_mod_20_22)


c(PRESS(inf_mod),PRESS(inf_mod_22_20))
c(PRESS_log_inf,PRESS_log_inf_22_20)


##
## weighted least squares 
##
x<-c(4.17, 5.58, 5.18, 6.11, 4.50, 4.61, 5.17, 4.53, 5.33, 5.14)
y<-c(18.10392, 25.93690, 24.54264, 26.39277, 10.06054, 20.07634,
     18.48421, 17.68286, 20.62061, 20.05093)	

plot(x,y)
model<-lm(y~x)
abline(model)

weight1 <- c(rep(1,4),10,rep(1,5))
model1<-lm(y~x,weight=weight1)
abline(model1,col = 'blue')

weight2 <- c(rep(1,4),1/10,rep(1,5))
model2<-lm(y~x,weight=weight2)
abline(model2,col = 'red')	

##
## EPS example (WLS)
##
EPS <- c(0.27,0.42, 0.39, 0.50, 0.57, 0.64, 0.76, 0.93, 1.03)
Year<-c(1:9)

#9x9 identity matrix
V <- diag(9)	
Wn <- c(1,1,1,14,6,87,9,6,7)

#put weights Wn as diagonal elements in V
diag(V) <- Wn      

#Fitting the weighted least squares model
gmodel<-lm(EPS~Year,weight = Wn)  
summary(gmodel)

#Fitting the generalized least squares model
library(MASS)
gmodel2<-lm.gls(EPS~Year,W = V)  
coefficients(gmodel2)

##
## Example: trees data (from R)
## 

trees

plot(Volume~Girth,data=trees) 

trees_mod <- lm(Volume~Girth,data=trees)

# diagnostic plots
par(mfrow=c(2,2))
plot(trees_mod)	
par(mfrow=c(1,1))

# model with quadratic term
trees$Girth2 <- trees$Girth^2
trees_mod2 <- lm(Volume~Girth+Girth2,data=trees)

par(mfrow=c(2,2))
plot(trees_mod2)
par(mfrow=c(1,1))

AIC(trees_mod,trees_mod2)

# Non linear transformations of predictors 

demo <- read.csv("demo.csv")

# Exponential relationship

plot(y1 ~ x1, data = demo)

model1 <- lm(log(y1) ~ x1, data = demo)
summary(model1)

plot(y1 ~ x1, data = demo)
curve(exp(model1$coefficients[1])*exp(model1$coefficients[2]*x), add=TRUE, col = "red")

# Parabolic relationship 

plot(y2 ~ x2, data = demo)

model2 <- lm(y2 ~ x2 + I(x2^2), data = demo)
summary(model2)

plot(y2 ~ x2, data = demo)
curve(model2$coefficients[1] + model2$coefficients[2] * x + model2$coefficients[3] * x^2, add=TRUE, col = "blue")


# Power relationship 

plot(y3 ~ x3, data = demo)

model3 <- lm(log(y3) ~ log(x3), data = demo)
summary(model3)

plot(y3 ~ x3, data = demo)
curve(exp(model3$coefficients[1])*x^model3$coefficients[2], add=TRUE, col = "green")

# Box-Cox transformation 

model4 <- lm(y1 ~ x1, data = demo)
c<-boxcox(model4, lambda=seq(-2,2,1/10)) 

c$x[!c$y<max(c$y)][1] 
lambda=c$x[!c$y<max(c$y)] 

plot((y1^lambda-1)/lambda~x1, data = demo)

model5 <- lm((y1^lambda-1)/lambda~x1, data = demo)
plot(y1 ~ x1, data = demo)
curve(((model5$coefficients[1] + model5$coefficients[2]*x)*lambda + 1)^(1/lambda), add = TRUE, col = "blue")
curve(exp(model1$coefficients[1])*exp(model1$coefficients[2]*x), add=TRUE, col = "red")
