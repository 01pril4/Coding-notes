##   
## Week 8 Lecture 2
##

##
## Example: rats data
##

rats <- read.table("rats.txt",header = TRUE)
rat_mod <- lm(y~Weight+Liver+Dose,data=rats)
summary(rat_mod)

summary(lm(y~Weight, data=rats))
summary(lm(y~Liver, data=rats))
summary(lm(y~Dose, data=rats))

#plot leverages
dotchart(hatvalues(rat_mod))

#plot externally studentised residuals
dotchart(abs(rstudent(rat_mod)))

#remove the third observation and refit
rat_modm3 <- lm(y~Weight+Liver+Dose,data=rats[-3,])
summary(rat_modm3)

#influence measures
influence.measures(rat_mod)

#plot Cook's distance and Residual vs Leverage
par(mfrow=c(1,2))
plot(rat_mod,c(4,5))
par(mfrow=c(1,1))

plot(rat_mod)

#how to extact influence measures 
infl = influence.measures(rat_mod)$infmat
colnames(infl)
dim(infl)

dotchart(infl[,"dfb.Livr"],xlab="dfb.Livr",ylab="observation")
dotchart(infl[,"dfb.Wght"],xlab="dfb.Wght",ylab="observation")
dotchart(infl[,"dfb.Dose"],xlab="dfb.Dose",ylab="observation")

#analysis: model without Liver
rat_mod2 <- lm(y~Weight+Dose,data=rats)
summary(rat_mod2)
influence.measures(rat_mod2)

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
