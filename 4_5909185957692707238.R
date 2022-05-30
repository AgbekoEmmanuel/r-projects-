library(moments)
library(EnvStats)
set.seed(1)
project<-rnorm(1000)
skewness(project)
hist(project)
lines(project,col="pink",lwd=4)
?skewness
######Computing skewness of random values using fishers coefficient of skewness##
skewness(project, method = "fisher", l.moment.method = "unbiased")
######Computing Skewness using the moments
skewness(project, method = "moment", l.moment.method = "unbiased")
#####Computing skewness using Karl Pearson's####
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
MEAN<-mean(project)
SD<-sd(project)
MEDIAN<-median(project)
MODE<-getmode(project)
Skp1<-3*(MEAN - MEDIAN)/SD
Skp2<-3*(MODE - MEDIAN)/SD

#####computing skewness using Bowley's###########
boxplot(project,horizontal = TRUE,col="lightblue")
?quantile
QUANTILES<- quantile(project,probs=c(0.25,0.50,0.75),type=6,names = FALSE)
Skb <- (QUANTILES[3]+QUANTILES[1]-2*QUANTILES[2])/(QUANTILES[3]-QUANTILES[1])

#######Kelly's coefficient of skewness###
DECILES<- quantile(project,probs=c(0.10,0.50,0.90),type=6,names = FALSE)
# box-plot of the data
boxplot(project,horizontal = TRUE,col="violet")
segments(y0=0.5,x0=DECILES[1],y1=1.5,x1=DECILES[1],col="green",lwd=4)
segments(y0=0.5,x0=DECILES[3],y1=1.5,x1=DECILES[3],col="yellow",lwd=4)
Sk <- (DECILES[3]+DECILES[1]-2*DECILES[2])/(DECILES[3]-DECILES[1])
#####MIDRANGE FORMULAE
MIN<-min(project)
MAX<-max(project)
###using the mode
####cannot be computed here since we generated random numbers 
##### involving the mean
Sm1<-(MIN+MAX-2*MEAN)/(MAX-MIN)
####Involving the median
Sm2<-(MIN+MAX-2*MEDIAN)/(MAX-MIN)

??biasfunction
??bias


# Create the function.
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

bias1 = function(mu,sigma,n,run){
  list.of.bias = rep(0,run)
  
  for (i in 1:run){
    data = rnorm(n,mu,sigma)
    Skewness.NP = (mean(data) - getmode(data))/sd(data)
    Skewness.Tr = skewness(data)
    
    Bias= Skewness.NP - Skewness.Tr
    list.of.bias[i] = Bias
  }
  
  print(mean(list.of.bias))
}

bias1(0,1,1000,1000)
?sd
mean(project)
mode(project)

bias2 = function(mu,sigma,n,run){
  list.of.bias = rep(0,run)
  
  for (i in 1:run){
    data = rnorm(n,mu,sigma)
    QUANTILES<- quantile(data,probs=c(0.25,0.50,0.75),type=6,names = FALSE)
    Skewness.NP = (QUANTILES[3]+QUANTILES[1]-2*QUANTILES[2])/(QUANTILES[3]-QUANTILES[1])
    Skewness.Tr = skewness(data)
    
    Bias= Skewness.NP - Skewness.Tr
    list.of.bias[i] = Bias
  }
  
  print(mean(list.of.bias))
}
bias2(0,1,1000,1000)


bias3 = function(mu,sigma,n,run){
  list.of.bias = rep(0,run)
  
  for (i in 1:run){
    data = rnorm(n,mu,sigma)
    DECILES<- quantile(data,probs=c(0.10,0.50,0.90),type=6,names = FALSE)
    Skewness.NP = (DECILES[3]+DECILES[1]-2*DECILES[2])/(DECILES[3]-DECILES[1])
    Skewness.Tr = skewness(data)
    
    Bias= Skewness.NP - Skewness.Tr
    list.of.bias[i] = Bias
  }
  
  print(mean(list.of.bias))
}
bias3(0,1,1000,1000)

bias4 = function(mu,sigma,n,run){
  list.of.bias = rep(0,run)
  
  for (i in 1:run){
    data = rnorm(n,mu,sigma)
    MIN<-min(data)
    MAX<-max(data)
    Skewness.NP = ((MIN+MAX-2*mean(data))/(MAX-MIN))
    Skewness.Tr = skewness(data)
    
    Bias= Skewness.NP - Skewness.Tr
    list.of.bias[i] = Bias
  }
  
  print(mean(list.of.bias))
}
bias4(0,1,1000,1000)

bias5 = function(mu,sigma,n,run){
  list.of.bias = rep(0,run)
  
  for (i in 1:run){
    data = rnorm(n,mu,sigma)
    MIN<-min(data)
    MAX<-max(data)
    Skewness.NP = ((MIN+MAX-2*median(data))/(MAX-MIN))
    Skewness.Tr = skewness(data)
    
    Bias= Skewness.NP - Skewness.Tr
    list.of.bias[i] = Bias
  }
  
  print(mean(list.of.bias))
}
bias5(0,1,1000,1000)


MSE1= var(project)-2*bias1(0,1,1000,1000)
MSE2= var(project)-2*bias2(0,1,1000,1000)
MSE3= var(project)-2*bias3(0,1,1000,1000)
MSE4= var(project)-2*bias4(0,1,1000,1000)
MSE5= var(project)-2*bias5(0,1,1000,1000)

