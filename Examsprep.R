library(MASS)
library(ISLR)
fix(Boston)
names(Boston)
?Boston
Dela=lm(medv~lstat)
Dela=lm(medv~lstat,data = Boston)
Dela
attach(Boston)
Dela=lm(medv~lstat)
Dela
summary(Dela)
names(Dela)
coef(Dela)
confint(Dela)
predict(Dela,data=Boston,interval = "confidence")
predict(Dela,interval = "confidence")
predict (Dela,data.frame(lstat=c(5,10 ,15)),interval ="confidence")
predict(Dela,Boston$lstat$c(5,10,15),interval="confidence")
predict (Dela,data.frame(lstat=c(5,10 ,15)),interval ="prediction")
plot(lstat,medv,main = "A plot of lstat against medv",xlab = 'lstat',ylab = 'medv')
abline(Dela)
abline (Dela,lwd =3)
abline (Dela,lwd=3,col ="red")
plot(lstat,medv ,col="red")
plot(lstat,medv ,pch =20)
plot(lstat,medv,pch ="+")
plot(1:20,1:20,pch =1:20)
par(mfrow=c(2,2))
plot(Dela)
residuals(Dela)
rstudent(Dela)
plot(predict (Dela), residuals (Dela))
plot(predict (Dela), rstudent (Dela))
hatvalues(Dela)
plot(hatvalues(Dela))
which.max(hatvalues(Dela))
which.min(hatvalues(Dela))

##### MLR####
Dela2=lm(medv~lstat+age ,data=Boston )
Dela2
summary(Dela2)
Dela3=lm(medv~.,data=Boston)
summary(Dela3)
?summary.lm
summary(Dela3)$r.sq
summary(Dela3)$sigma
vif(Dela3)
lm.fit1=lm(medv.-age ,data=Boston )
summary (lm.fit1)


#####Logistic Regression####
library(ISLR)
names(Smarket)
dim(Smarket)
summary(Smarket)
pairs(Smarket)
cor(Smarket)
cor(Smarket[,-9])
attach(Smarket)
plot(Volume)
glm.fits=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,data=Smarket ,family=binomial )
summary(glm.fits)
coef(glm.fits)
summary(glm.fits)$coef
summary(glm.fits)$coef[,4]
glm.probs=predict (glm.fits,type="response")
glm.probs [1:10]
contrasts(Direction)
glm.pred=rep("Down" ,1250)
glm.pred[glm.probs >.5]="Up"
table(glm.pred ,Direction )
(507+145) /1250
mean(glm.pred==Direction)
train=(Year<2005)
Smarket.2005=Smarket[!train,]
dim(Smarket.2005)
Direction.2005=Direction[!train]
glm.fits=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,data=Smarket,family=binomial,subset=train)
glm.probs=predict(glm.fits,Smarket.2005,type="response")
