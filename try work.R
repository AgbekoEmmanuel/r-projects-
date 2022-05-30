library(MASS)
library(ISLR)
fix(Boston)
names(Boston)
?Boston
lm.fit=lm(medv~lstat)
lm.fit=lm(medv~lstat,data=Boston)
attach(Boston)
lm.fit=lm(medv~lstat)
summary(lm.fit)
names(lm.fit)
lm.fit$coefficients
coef(lm.fit)
confint(lm.fit)
predict(lm.fit,data.frame(lstat=c(5,10,15)),interval = "confidence")
predict(lm.fit,data.frame(lstat=c(5,10,15)),interval = "prediction")
plot(lstat,medv)
abline(lm.fit)
abline(a,b)
abline(lm.fit,lwd=3)
plot(lm.fit,lwd=3,col="green")
plot(lm.fit,lwd=3,col="green",pch=20)
plot(lstat,mdev,pch="+")
par(mfrow=c(2,2))
plot(lm.fit)
residuals(lm.fit)
rstudent(lm.fit)
plot(predict(lm.fit),residuals(lm.fit))
plot(predict(lm.fit),rstudent(lm.fit))
plot(hatvalues(lm.fit))
which.max(hatvalues(lm.fit))
lm.fit1=lm(medv~lstat+age,data = Boston)
summary(lm.fit1)
lm.fit2=lm(medv~.,data = Boston)
summary(lm.fit2)
?summary.lm
summary(lm.fit2)$r.sq
summary(lm.fit2)$sigma
lim.fit3=lm(medv~.-age,data = Boston)
summary(lim.fit3)
library(carData)
vif(lm.fit1)
lim.fit1=update(lm.fit,~-age)
lstat:black
lstat*age
summary(lm(medv~lstat*age,data = Boston))
lm.fit4=lm(medv~lstat+I(lstat^2))
summary(lm.fit4)
lm.fit=lm(medv~lstat)
anova(lm.fit,lm.fit4)
par(mfrow=c(2,2))
plot(lm.fit4)
lm.fit5=lm(medv~poly(lstat ,5))
summary(lm.fit5)
summary (lm(medv~log(rm),data=Boston))
fix(Carseats)
names(Carseats)
lm.fit6=lm(Sales~.+Income :Advertising +Price:Age ,data=Carseats )
summary(lm.fit6)
attach(Carseats)
contrasts(ShelveLoc)
?contrasts
LoadLibraries
LoadLibraries()
LoadLibraries()
function(){
  library(ISRL)
  library(MASS)
  print("The libraries have been loaded.")
}
LoadLibraries()
LoadLibraries
function (){
  library(ISLR)
  library(MASS)
  print("The libraries have been loaded .")
}
LoadLibraries()
LoadLibraries()
function (){
  library(ISLR)
  library(MASS)
  print("The libraries have been loaded .")
}
LoadLibraries()


###boostsrap
