###a)
library(ISLR)
library(Weekly)
names(Weekly)
dim(Weekly)
summary(Weekly)
pairs(Weekly)
cor(Weekly[,-9])
attach(Weekly)
plot(Volume)

##b)
dela.fits=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,data = Weekly,family=binomial)
summary(dela.fits)
### Lag1,Lag2 and Lag4 appears to be statistically significant
?Weekly
##c)
glm.probs=predict (dela.fits,type="response")
glm.probs [1:10]
contrasts (Direction )
glm.pred=rep("Down",1089)
glm.pred[glm.probs>.5]="Up"
table(glm.pred,Direction)
(54+557)/1089
mean(glm.pred==Direction)
###d)
train=(Year<2009)
Weekly.2009= Weekly[!train,]
dim(Weekly.2009)
Direction.2009=Direction[!train]
dela.fits2=glm(Direction~Lag2,data=Weekly,family=binomial,subset=train)
summary(dela.fits2)
glm.probs=predict(dela.fits,Weekly.2009,type="response")
dela.fits2=rep("Down",104)
dela.fits2[glm.probs >.5]="Up"
table(dela.fits2,Direction.2009)
(9+56)/1089

####e)

library(MASS)
dela.fit3=lda(Direction~Lag2,data=Weekly,subset=train)
dela.fit3
dela.fit3=predict(dela.fit3,Weekly.2009)
table(dela.fit3$class,Direction.2009)

###f)
dela.fit4=qda(Direction~Lag2,data=Weekly,subset=train)
dela.fit4
dela.class=predict(dela.fit4 ,Weekly.2009)$class
table(dela.class,Direction.2009)
(0+61)/1089

###g
library(class)
train.X=as.matrix(Lag2[train])
test.X=as.matrix(Lag2[!train])
train.Direction=Direction[train]
set.seed(1)
dela.fit5=knn(train.X,test.X,train.Direction,k=1)
dela.class=predict(dela.fit5 ,Weekly.2009)$class
table(dela.fit5,Direction.2009)
(21+31)/1089


###i)
mean(dela.fit3==Direction.2009)
mean(dela.fit4==Direction.2009)
mean(dela.fit5==Direction.2009)