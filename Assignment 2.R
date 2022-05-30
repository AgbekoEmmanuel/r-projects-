attach(Auto)
fix(Auto)
Auto=na.omit(Auto)
Q1=lm(mpg~horsepower,data = Auto)
summary(Q1)
##i)the p-value is 0.00000022507 which is very small enough so we reject the null hypothesis which states that there is no relationship between the mpg and horsepower.Hence there is exist a relationship between mpg and horsepower.

##ii) mpg can be predicted with a high level of accuracy hence there is a strong relationship between mpg and horsepower



predict(Q1,data.frame(horsepower=98),interval = "confidence")
predict(Q1,data.frame(horsepower=98),interval = "prediction")

plot(horsepower,mpg,xlab = "horsepower",ylab = "mpg",main = "A plot of mpg against horsepower ")
abline(Q1)

par(mfrow=c(2,2))
plot(Q1)
