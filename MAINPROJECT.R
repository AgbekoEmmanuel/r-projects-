library(moments)
library(EnvStats)

#Fishers with n=10, mean=0, sd=1
set.seed(1)
project1=matrix(sample(rnorm(10,mean=0,sd=1),1000,replace=TRUE),ncol=10)
skewness(project1[1,])


skew <- vector("numeric", nrow(project1))  
for (i in 1:nrow(project1)){
  skew[i] <- skewness(project1[i, ], na.rm=TRUE) 
}
setNames(skew, rownames(project1))



xbar1=mean(skew,trim=0,na.rm=FALSE)

MSE1= ((xbar1)^2)/100


# Fishers with n=100, mean=0 and sd=1
set.seed(1)
project2=matrix(sample(rnorm(100,mean=0,sd=1),10000,replace=TRUE),ncol=100)
skewness(project2[1,])


skew2 <- vector("numeric", nrow(project2))  
for (i in 1:nrow(project2)){
  skew2[i] <- skewness(project2[i, ], na.rm=TRUE) 
}
setNames(skew2, rownames(project2))



xbar2=mean(skew2,trim=0,na.rm=FALSE)

MSE2= ((xbar2)^2)/100


# Fishers with n=1000, mean=0 and sd=1

set.seed(1)
project3=matrix(sample(rnorm(1000,mean=0,sd=1),100000,replace=TRUE),ncol=1000)
skewness(project3[1,])


skew3 <- vector("numeric", nrow(project3))  
for (i in 1:nrow(project3)){
  skew3[i] <- skewness(project3[i, ], na.rm=TRUE) 
}
setNames(skew3, rownames(project3))



xbar3=mean(skew3,trim=0,na.rm=FALSE)

MSE3= ((xbar3)^2)/100





#Fishers with n=10, mean=0, sd=2
set.seed(1)
project4=matrix(sample(rnorm(10,mean=0,sd=2),1000,replace=TRUE),ncol=10)
skewness(project4[1,])


skew4 <- vector("numeric", nrow(project4))  
for (i in 1:nrow(project4)){
  skew4[i] <- skewness(project4[i, ], na.rm=TRUE) 
}
setNames(skew4, rownames(project4))



xbar4=mean(skew4,trim=0,na.rm=FALSE)

MSE4= ((xbar4)^2)/100


# Fishers with n=100, mean=0 and sd=2
set.seed(1)
project5=matrix(sample(rnorm(100,mean=0,sd=2),10000,replace=TRUE),ncol=100)
skewness(project5[1,])



skew5 <- vector("numeric", nrow(project5))  
for (i in 1:nrow(project5)){
  skew5[i] <- skewness(project5[i, ], na.rm=TRUE) 
}
setNames(skew5, rownames(project5))



xbar5=mean(skew5,trim=0,na.rm=FALSE)

MSE5= ((xbar5)^2)/100


# Fishers with n=1000, mean=0 and sd=2

set.seed(1)
project6=matrix(sample(rnorm(1000,mean=0,sd=2),100000,replace=TRUE),ncol=1000)
skewness(project6[1,])


skew6 <- vector("numeric", nrow(project6))  
for (i in 1:nrow(project6)){
  skew6[i] <- skewness(project6[i, ], na.rm=TRUE) 
}
setNames(skew6, rownames(project6))



xbar6=mean(skew6,trim=0,na.rm=FALSE)

MSE6= ((xbar6)^2)/100



#Fishers with n=10, mean=0, sd=3
set.seed(1)
project7=matrix(sample(rnorm(10,mean=0,sd=3),1000,replace=TRUE),ncol=10)
skewness(project1[1,])


skew7 <- vector("numeric", nrow(project7))  
for (i in 1:nrow(project1)){
  skew[i] <- skewness(project7[i, ], na.rm=TRUE) 
}
setNames(skew7, rownames(project7))



xbar7=mean(skew7,trim=0,na.rm=FALSE)

MSE7= ((xbar7)^2)/100


# Fishers with n=100, mean=0 and sd=3
set.seed(1)
project8=matrix(sample(rnorm(100,mean=0,sd=3),10000,replace=TRUE),ncol=100)
skewness(project8[1,])


skew8 <- vector("numeric", nrow(project2))  
for (i in 1:nrow(project8)){
  skew8[i] <- skewness(project8[i, ], na.rm=TRUE) 
}
setNames(skew8, rownames(project8))



xbar8=mean(skew8,trim=0,na.rm=FALSE)

MSE8= ((xbar8)^2)/100


# Fishers with n=1000, mean=0 and sd=3

set.seed(1)
project9=matrix(sample(rnorm(1000,mean=0,sd=3),100000,replace=TRUE),ncol=1000)
skewness(project9[1,])


skew9 <- vector("numeric", nrow(project9))  
for (i in 1:nrow(project9)){
  skew9[i] <- skewness(project9[i, ], na.rm=TRUE) 
}
setNames(skew9, rownames(project9))



xbar9=mean(skew9,trim=0,na.rm=FALSE)

MSE9= ((xbar9)^2)/100


####karl Pearson with n=10, mean =0 ,sd=1
set.seed(1)
project10=matrix(sample(rnorm(10,mean=0,sd=1),1000,replace=TRUE),ncol=10)
mean(project10[i,])
median(project10[i,])
sd(project10[i,])
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

skew10 <- vector("numeric", nrow(project10))  
for (i in 1:nrow(project10)){
  skew10[i] <- skewness(project10[i, ], na.rm=TRUE) 
  skewness.k10 <-3*(mean(project10[i,])-median(project10[i,]))/sd(project10[i,])
}
setNames(skew10, rownames(project10))

xbar10=mean(skew10,trim=0,na.rm=FALSE)

MSE10= ((xbar10)^2)/100




####karl Pearson with n=100, mean =0 ,sd=1
set.seed(1)
project11=matrix(sample(rnorm(100,mean=0,sd=1),10000,replace=TRUE),ncol=100)
mean(project11[i,])
median(project11[i,])
sd(project11[i,])
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

skew11 <- vector("numeric", nrow(project11))  
for (i in 1:nrow(project11)){
  skew11[i] <- skewness(project11[i, ], na.rm=TRUE) 
  skewness.k11 <-3*(mean(project11[i,])-median(project11[i,]))/sd(project11[i,])
}
setNames(skew11, rownames(project11))

xbar11=mean(skew11,trim=0,na.rm=FALSE)

MSE11= ((xbar11)^2)/100






####karl Pearson with n=1000, mean =0 ,sd=1
set.seed(1)
project12=matrix(sample(rnorm(1000,mean=0,sd=1),100000,replace=TRUE),ncol=1000)
mean(project12[i,])
median(project12[i,])
sd(project12[i,])
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

skew12<- vector("numeric", nrow(project12))  
for (i in 1:nrow(project12)){
  skew12[i] <- skewness(project12[i, ], na.rm=TRUE) 
  skewness.k12 <-3*(getmode(project12[i,])-median(project12[i,]))/sd(project12[i,])
}
setNames(skew12, rownames(project12))

xbar12=mean(skew12,trim=0,na.rm=FALSE)

MSE12= ((xbar12)^2)/100



####karl Pearson with n=10, mean =0 ,sd=2
set.seed(1)
project13=matrix(sample(rnorm(10,mean=0,sd=2),1000,replace=TRUE),ncol=10)
mean(project13[i,])
median(project13[i,])
sd(project13[i,])
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

skew13 <- vector("numeric", nrow(project13))  
for (i in 1:nrow(project13)){
  skew13[i] <- skewness(project13[i, ], na.rm=TRUE) 
  skewness.k13 <-3*(mean(project13[i,])-median(project13[i,]))/sd(project13[i,])
}
setNames(skew13, rownames(project13))

xbar13=mean(skew13,trim=0,na.rm=FALSE)

MSE13= ((xbar13)^2)/100




####karl Pearson with n=100, mean =0 ,sd=2
set.seed(1)
project14=matrix(sample(rnorm(100,mean=0,sd=2),10000,replace=TRUE),ncol=100)
mean(project14[i,])
median(project14[i,])
sd(project14[i,])
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

skew14 <- vector("numeric", nrow(project14))  
for (i in 1:nrow(project14)){
  skew14[i] <- skewness(project14[i, ], na.rm=TRUE) 
  skewness.k14 <-3*(mean(project14[i,])-median(project14[i,]))/sd(project14[i,])
}
setNames(skew14, rownames(project14))

xbar14=mean(skew14,trim=0,na.rm=FALSE)

MSE14= ((xbar14)^2)/100



####karl Pearson with n=1000, mean =0 ,sd=2
set.seed(1)
project15=matrix(sample(rnorm(1000,mean=0,sd=2),100000,replace=TRUE),ncol=1000)
mean(project15[i,])
median(project15[i,])
sd(project15[i,])
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

skew15<- vector("numeric", nrow(project15))  
for (i in 1:nrow(project15)){
  skew15[i] <- skewness(project15[i, ], na.rm=TRUE) 
  skewness.k15 <-3*(getmode(project15[i,])-median(project15[i,]))/sd(project15[i,])
}
setNames(skew15, rownames(project15))

xbar15=mean(skew15,trim=0,na.rm=FALSE)

MSE15= ((xbar15)^2)/100









####karl Pearson with n=10, mean =0 ,sd=3
set.seed(1)
project16=matrix(sample(rnorm(10,mean=0,sd=3),1000,replace=TRUE),ncol=10)
mean(project16[i,])
median(project16[i,])
sd(project16[i,])
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

skew16 <- vector("numeric", nrow(project16))  
for (i in 1:nrow(project16)){
  skew16[i] <- skewness(project16[i, ], na.rm=TRUE) 
  skewness.k16 <-3*(mean(project16[i,])-median(project16[i,]))/sd(project16[i,])
}
setNames(skew16, rownames(project16))

xbar16=mean(skew16,trim=0,na.rm=FALSE)

MSE16= ((xbar16)^2)/100




####karl Pearson with n=100, mean =0 ,sd=3
set.seed(1)
project17=matrix(sample(rnorm(100,mean=0,sd=3),10000,replace=TRUE),ncol=100)
mean(project17[i,])
median(project17[i,])
sd(project17[i,])
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

skew17 <- vector("numeric", nrow(project17))  
for (i in 1:nrow(project17)){
  skew17[i] <- skewness(project17[i, ], na.rm=TRUE) 
  skewness.k17 <-3*(mean(project17[i,])-median(project17[i,]))/sd(project17[i,])
}
setNames(skew17, rownames(project17))

xbar17=mean(skew17,trim=0,na.rm=FALSE)

MSE17= ((xbar17)^2)/100



####karl Pearson with n=1000, mean =0 ,sd=3
set.seed(1)
project18=matrix(sample(rnorm(1000,mean=0,sd=3),100000,replace=TRUE),ncol=1000)
mean(project18[i,])
median(project18[i,])
sd(project18[i,])
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

skew18<- vector("numeric", nrow(project18))  
for (i in 1:nrow(project18)){
  skew18[i] <- skewness(project18[i, ], na.rm=TRUE) 
  skewness.k18 <-3*(getmode(project18[i,])-median(project18[i,]))/sd(project18[i,])
}
setNames(skew18, rownames(project18))

xbar18=mean(skew18,trim=0,na.rm=FALSE)

MSE18= ((xbar18)^2)/100
















######BOWLEY with n=10,mean=0,sd=1
set.seed(1)
project19=matrix(sample(rnorm(10,mean=0,sd=1),1000,replace=TRUE),ncol=10)
QUANTILE19<-quantile(project19[1,],probs = c(0.25,0.5,0.75),type = 6,names = FALSE)
QUANTILE19[2]
skew19 <- vector("numeric", nrow(project19))  
for (i in 1:nrow(project19)){
  skew19[i] <- skewness(project19[i, ], na.rm=TRUE) 
  QUANTILES19<- quantile(project19[i,],probs=c(0.25,0.50,0.75),type=6,names = FALSE)
  skewness.M19 <-(QUANTILE19[3]+QUANTILE19[1]-2*QUANTILE19[2])/(QUANTILE19[3]-QUANTILE19[1])
}
setNames(skew19, rownames(project19))

xbar19=mean(skew19,trim=0,na.rm=FALSE)

MSE19= ((xbar19)^2)/100




######BOWLEY with n=100,mean=0,sd=1
set.seed(1)
project20=matrix(sample(rnorm(100,mean=0,sd=1),10000,replace=TRUE),ncol=100)
QUANTILE20<-quantile(project20[1,],probs = c(0.25,0.5,0.75),type = 6,names = FALSE)
QUANTILE20[2]
skew20 <- vector("numeric", nrow(project20))  
for (i in 1:nrow(project20)){
  skew20[i] <- skewness(project20[i, ], na.rm=TRUE) 
  QUANTILES20<- quantile(project20[i,],probs=c(0.25,0.50,0.75),type=6,names = FALSE)
  skewness.M20 <-(QUANTILE20[3]+QUANTILE20[1]-2*QUANTILE20[2])/(QUANTILE20[3]-QUANTILE20[1])
}
setNames(skew20, rownames(project20))

xbar20=mean(skew20,trim=0,na.rm=FALSE)

MSE20= ((xbar20)^2)/100


######BOWLEY with n=1000,mean=0,sd=1
set.seed(1)
project21=matrix(sample(rnorm(1000,mean=0,sd=1),100000,replace=TRUE),ncol=1000)
QUANTILE21<-quantile(project21[1,],probs = c(0.25,0.5,0.75),type = 6,names = FALSE)
QUANTILE21[2]
skew21 <- vector("numeric", nrow(project21))  
for (i in 1:nrow(project21)){
  skew21[i] <- skewness(project21[i, ], na.rm=TRUE) 
  QUANTILES21<- quantile(project21[i,],probs=c(0.25,0.50,0.75),type=6,names = FALSE)
  skewness.M21 <-(QUANTILE21[3]+QUANTILE21[1]-2*QUANTILE21[2])/(QUANTILE21[3]-QUANTILE21[1])
}
setNames(skew21, rownames(project21))

xbar21=mean(skew21,trim=0,na.rm=FALSE)

MSE21= ((xbar21)^2)/100





######BOWLEY with n=10,mean=0,sd=2
set.seed(1)
project22=matrix(sample(rnorm(10,mean=0,sd=2),1000,replace=TRUE),ncol=10)
QUANTILE22<-quantile(project22[1,],probs = c(0.25,0.5,0.75),type = 6,names = FALSE)
QUANTILE22[2]
skew22 <- vector("numeric", nrow(project22))  
for (i in 1:nrow(project22)){
  skew22[i] <- skewness(project22[i, ], na.rm=TRUE) 
  QUANTILES22<- quantile(project22[i,],probs=c(0.25,0.50,0.75),type=6,names = FALSE)
  skewness.M22 <-(QUANTILE22[3]+QUANTILE22[1]-2*QUANTILE22[2])/(QUANTILE22[3]-QUANTILE22[1])
}
setNames(skew22, rownames(project22))

xbar22=mean(skew22,trim=0,na.rm=FALSE)

MSE22= ((xbar22)^2)/100




######BOWLEY with n=100,mean=0,sd=2
set.seed(1)
project23=matrix(sample(rnorm(100,mean=0,sd=2),10000,replace=TRUE),ncol=100)
QUANTILE23<-quantile(project23[1,],probs = c(0.25,0.5,0.75),type = 6,names = FALSE)
QUANTILE23[2]
skew23 <- vector("numeric", nrow(project23))  
for (i in 1:nrow(project23)){
  skew23[i] <- skewness(project23[i, ], na.rm=TRUE) 
  QUANTILES23<- quantile(project23[i,],probs=c(0.25,0.50,0.75),type=6,names = FALSE)
  skewness.M23 <-(QUANTILE23[3]+QUANTILE23[1]-2*QUANTILE23[2])/(QUANTILE23[3]-QUANTILE23[1])
}
setNames(skew23, rownames(project23))

xbar23=mean(skew23,trim=0,na.rm=FALSE)

MSE23= ((xbar23)^2)/100


######BOWLEY with n=1000,mean=0,sd=2
set.seed(1)
project24=matrix(sample(rnorm(1000,mean=0,sd=2),100000,replace=TRUE),ncol=1000)
QUANTILE24<-quantile(project24[1,],probs = c(0.25,0.5,0.75),type = 6,names = FALSE)
QUANTILE24[2]
skew24 <- vector("numeric", nrow(project24))  
for (i in 1:nrow(project24)){
  skew24[i] <- skewness(project24[i, ], na.rm=TRUE) 
  QUANTILES24<- quantile(project24[i,],probs=c(0.25,0.50,0.75),type=6,names = FALSE)
  skewness.M24 <-(QUANTILE24[3]+QUANTILE24[1]-2*QUANTILE24[2])/(QUANTILE24[3]-QUANTILE24[1])
}
setNames(skew24, rownames(project24))

xbar24=mean(skew24,trim=0,na.rm=FALSE)

MSE24= ((xbar24)^2)/100





######BOWLEY with n=10,mean=0,sd=3
set.seed(1)
project25=matrix(sample(rnorm(10,mean=0,sd=3),1000,replace=TRUE),ncol=10)
QUANTILE25<-quantile(project25[1,],probs = c(0.25,0.5,0.75),type = 6,names = FALSE)
QUANTILE25[2]
skew25 <- vector("numeric", nrow(project25))  
for (i in 1:nrow(project25)){
  skew25[i] <- skewness(project25[i, ], na.rm=TRUE) 
  QUANTILES25<- quantile(project25[i,],probs=c(0.25,0.50,0.75),type=6,names = FALSE)
  skewness.M25 <-(QUANTILE25[3]+QUANTILE25[1]-2*QUANTILE25[2])/(QUANTILE25[3]-QUANTILE25[1])
}
setNames(skew25, rownames(project25))

xbar25=mean(skew25,trim=0,na.rm=FALSE)

MSE25= ((xbar25)^2)/100




######BOWLEY with n=100,mean=0,sd=3
set.seed(1)
project26=matrix(sample(rnorm(100,mean=0,sd=3),10000,replace=TRUE),ncol=100)
QUANTILE26<-quantile(project26[1,],probs = c(0.25,0.5,0.75),type = 6,names = FALSE)
QUANTILE26[2]
skew26 <- vector("numeric", nrow(project26))  
for (i in 1:nrow(project26)){
  skew26[i] <- skewness(project26[i, ], na.rm=TRUE) 
  QUANTILES26<- quantile(project26[i,],probs=c(0.25,0.50,0.75),type=6,names = FALSE)
  skewness.M26 <-(QUANTILE26[3]+QUANTILE26[1]-2*QUANTILE26[2])/(QUANTILE26[3]-QUANTILE26[1])
}
setNames(skew26, rownames(project26))

xbar26=mean(skew26,trim=0,na.rm=FALSE)

MSE26= ((xbar26)^2)/100


######BOWLEY with n=1000,mean=0,sd=3
set.seed(1)
project27=matrix(sample(rnorm(1000,mean=0,sd=3),100000,replace=TRUE),ncol=1000)
QUANTILE27<-quantile(project27[1,],probs = c(0.25,0.5,0.75),type = 6,names = FALSE)
QUANTILE27[2]
skew27 <- vector("numeric", nrow(project27))  
for (i in 1:nrow(project27)){
  skew27[i] <- skewness(project27[i, ], na.rm=TRUE) 
  QUANTILES27<- quantile(project27[i,],probs=c(0.25,0.50,0.75),type=6,names = FALSE)
  skewness.M27 <-(QUANTILE27[3]+QUANTILE27[1]-2*QUANTILE27[2])/(QUANTILE27[3]-QUANTILE27[1])
}
setNames(skew27, rownames(project27))

xbar27=mean(skew27,trim=0,na.rm=FALSE)

MSE27= ((xbar27)^2)/100







####### KELLY WITH N=10, MEAN = 0 AND SD=1
set.seed(1)
project28=matrix(sample(rnorm(10,mean=0,sd=1),1000,replace=TRUE),ncol=10)
DECILE28=quantile(project28[1,],probs=c(0.10,0.50,0.90),type=6,names = FALSE)
skew28 <- vector("numeric", nrow(project28))  
for (i in 1:nrow(project28)){
  skew28[i] <- skewness(project28[i, ], na.rm=TRUE) 
  DECILE28=quantile(project28[i,],probs=c(0.10,0.50,0.90),type=6,names = FALSE)
  skewness.M28 <-(DECILE28[3]+DECILE28[1]-2*DECILE28[2])/(DECILE28[3]-DECILE28[1])
}
setNames(skew28, rownames(project28))

xbar28=mean(skew28,trim=0,na.rm=FALSE)

MSE28= ((xbar28)^2)/100



####### KELLY WITH N=100, MEAN = 0 AND SD=1
set.seed(1)
project29=matrix(sample(rnorm(100,mean=0,sd=1),10000,replace=TRUE),ncol=100)
DECILE29=quantile(project29[1,],probs=c(0.10,0.50,0.90),type=6,names = FALSE)
skew29 <- vector("numeric", nrow(project29))  
for (i in 1:nrow(project29)){
  skew29[i] <- skewness(project29[i, ], na.rm=TRUE) 
  DECILE29=quantile(project29[i,],probs=c(0.10,0.50,0.90),type=6,names = FALSE)
  skewness.M29 <-(DECILE29[3]+DECILE29[1]-2*DECILE29[2])/(DECILE29[3]-DECILE29[1])
}
setNames(skew29, rownames(project29))

xbar29=mean(skew29,trim=0,na.rm=FALSE)

MSE29= ((xbar29)^2)/100



####### KELLY WITH N=1000, MEAN = 0 AND SD=1
set.seed(1)
project30=matrix(sample(rnorm(1000,mean=0,sd=1),100000,replace=TRUE),ncol=1000)
DECILE30=quantile(project30[1,],probs=c(0.10,0.50,0.90),type=6,names = FALSE)
skew30 <- vector("numeric", nrow(project30))  
for (i in 1:nrow(project30)){
  skew30[i] <- skewness(project30[i, ], na.rm=TRUE) 
  DECILE30=quantile(project30[i,],probs=c(0.10,0.50,0.90),type=6,names = FALSE)
  skewness.M30 <-(DECILE30[3]+DECILE30[1]-2*DECILE30[2])/(DECILE30[3]-DECILE30[1])
}
setNames(skew30, rownames(project30))

xbar30=mean(skew30,trim=0,na.rm=FALSE)

MSE30= ((xbar30)^2)/100






####### KELLY WITH N=10, MEAN = 0 AND SD=2
set.seed(1)
project31=matrix(sample(rnorm(10,mean=0,sd=2),1000,replace=TRUE),ncol=10)
DECILE31=quantile(project31[1,],probs=c(0.10,0.50,0.90),type=6,names = FALSE)
skew31 <- vector("numeric", nrow(project31))  
for (i in 1:nrow(project31)){
  skew31[i] <- skewness(project31[i, ], na.rm=TRUE) 
  DECILE31=quantile(project31[i,],probs=c(0.10,0.50,0.90),type=6,names = FALSE)
  skewness.M31 <-(DECILE31[3]+DECILE31[1]-2*DECILE31[2])/(DECILE31[3]-DECILE31[1])
}
setNames(skew31, rownames(project31))

xbar31=mean(skew31,trim=0,na.rm=FALSE)

MSE31= ((xbar31)^2)/100



####### KELLY WITH N=100, MEAN = 0 AND SD=2
set.seed(1)
project32=matrix(sample(rnorm(100,mean=0,sd=2),10000,replace=TRUE),ncol=100)
DECILE32=quantile(project32[1,],probs=c(0.10,0.50,0.90),type=6,names = FALSE)
skew32 <- vector("numeric", nrow(project32))  
for (i in 1:nrow(project32)){
  skew32[i] <- skewness(project32[i, ], na.rm=TRUE) 
  DECILE32=quantile(project32[i,],probs=c(0.10,0.50,0.90),type=6,names = FALSE)
  skewness.M32 <-(DECILE32[3]+DECILE32[1]-2*DECILE32[2])/(DECILE32[3]-DECILE32[1])
}
setNames(skew32, rownames(project32))

xbar32=mean(skew32,trim=0,na.rm=FALSE)

MSE32= ((xbar32)^2)/100



####### KELLY WITH N=1000, MEAN = 0 AND SD=2
set.seed(1)
project33=matrix(sample(rnorm(1000,mean=0,sd=2),100000,replace=TRUE),ncol=1000)
DECILE33=quantile(project33[1,],probs=c(0.10,0.50,0.90),type=6,names = FALSE)
skew33 <- vector("numeric", nrow(project33))  
for (i in 1:nrow(project33)){
  skew33[i] <- skewness(project33[i, ], na.rm=TRUE) 
  DECILE33=quantile(project33[i,],probs=c(0.10,0.50,0.90),type=6,names = FALSE)
  skewness.M33 <-(DECILE33[3]+DECILE33[1]-2*DECILE33[2])/(DECILE33[3]-DECILE33[1])
}
setNames(skew33, rownames(project33))

xbar33=mean(skew33,trim=0,na.rm=FALSE)

MSE33= ((xbar33)^2)/100





####### KELLY WITH N=10, MEAN = 0 AND SD=3
set.seed(1)
project34=matrix(sample(rnorm(10,mean=0,sd=3),1000,replace=TRUE),ncol=10)
DECILE34=quantile(project34[1,],probs=c(0.10,0.50,0.90),type=6,names = FALSE)
skew34 <- vector("numeric", nrow(project34))  
for (i in 1:nrow(project34)){
  skew34[i] <- skewness(project34[i, ], na.rm=TRUE) 
  DECILE34=quantile(project34[i,],probs=c(0.10,0.50,0.90),type=6,names = FALSE)
  skewness.M34 <-(DECILE34[3]+DECILE34[1]-2*DECILE34[2])/(DECILE34[3]-DECILE34[1])
}
setNames(skew34, rownames(project34))

xbar34=mean(skew34,trim=0,na.rm=FALSE)

MSE34= ((xbar34)^2)/100



####### KELLY WITH N=100, MEAN = 0 AND SD=3
set.seed(1)
project35=matrix(sample(rnorm(100,mean=0,sd=3),10000,replace=TRUE),ncol=100)
DECILE35=quantile(project35[1,],probs=c(0.10,0.50,0.90),type=6,names = FALSE)
skew35 <- vector("numeric", nrow(project35))  
for (i in 1:nrow(project35)){
  skew35[i] <- skewness(project35[i, ], na.rm=TRUE) 
  DECILE35=quantile(project35[i,],probs=c(0.10,0.50,0.90),type=6,names = FALSE)
  skewness.M35 <-(DECILE35[3]+DECILE35[1]-2*DECILE35[2])/(DECILE35[3]-DECILE35[1])
}
setNames(skew35, rownames(project35))

xbar35=mean(skew35,trim=0,na.rm=FALSE)

MSE35= ((xbar35)^2)/100



####### KELLY WITH N=1000, MEAN = 0 AND SD=3
set.seed(1)
project36=matrix(sample(rnorm(1000,mean=0,sd=3),100000,replace=TRUE),ncol=1000)
DECILE36=quantile(project36[1,],probs=c(0.10,0.50,0.90),type=6,names = FALSE)
skew36 <- vector("numeric", nrow(project36))  
for (i in 1:nrow(project36)){
  skew36[i] <- skewness(project36[i, ], na.rm=TRUE) 
  DECILE36=quantile(project36[i,],probs=c(0.10,0.50,0.90),type=6,names = FALSE)
  skewness.M36 <-(DECILE36[3]+DECILE36[1]-2*DECILE36[2])/(DECILE36[3]-DECILE36[1])
}
setNames(skew36, rownames(project36))

xbar36=mean(skew36,trim=0,na.rm=FALSE)

MSE36= ((xbar36)^2)/100








#####MIDRANGE FORMULAE INVOLVING THE MEAN WITH N=10, MEAN=0 AND SD=1
set.seed(1)
project37=matrix(sample(rnorm(10,mean=0,sd=1),1000,replace=TRUE),ncol=10)
MIN37<-min(project37[1,])
MAX37<-max(project37[1,])
MEAN37=mean(project37[1,])
####INVOLVING THE MEAN
skew37 <- vector("numeric", nrow(project37))  
for (i in 1:nrow(project37)){
  skew37[i] = skewness(project37[i, ], na.rm=TRUE) 
  MIN37= min(project37[i,])
  MAX37= max(project37[i,])
  MEAN37=mean(project37[i,])
  skewness.M37=(MIN37+MAX37-2*MEAN37)/(MAX37-MIN37)
}
setNames(skew37, rownames(project37))

xbar37=mean(skew37,trim=0,na.rm=FALSE)

MSE37= ((xbar37)^2)/100

####INVOLVING THE MEDIAN 
skew38 <- vector("numeric", nrow(project37))  
for (i in 1:nrow(project37)){
  skew38[i] = skewness(project37[i, ], na.rm=TRUE) 
  MIN37= min(project37[i,])
  MAX37= max(project37[i,])
  MEDIAN37=median(project37[i,])
  skewness.M37=(MIN37+MAX37-2*MEDIAN37)/(MAX37-MIN37)
}
setNames(skew38, rownames(project37))

xbar38=mean(skew38,trim=0,na.rm=FALSE)

MSE38= ((xbar38)^2)/100




#####MIDRANGE FORMULAE INVOLVING THE MEAN WITH N=100, MEAN=0 AND SD=1
set.seed(1)
project39=matrix(sample(rnorm(100,mean=0,sd=1),10000,replace=TRUE),ncol=100)
MIN39<-min(project39[1,])
MAX39<-max(project39[1,])
MEAN39=mean(project39[1,])
####INVOLVING THE MEAN
skew39 <- vector("numeric", nrow(project39))  
for (i in 1:nrow(project39)){
  skew39[i] = skewness(project39[i, ], na.rm=TRUE) 
  MIN39= min(project39[i,])
  MAX39= max(project39[i,])
  MEAN39=mean(project39[i,])
  skewness.M39=(MIN39+MAX39-2*MEAN39)/(MAX39-MIN39)
}
setNames(skew39, rownames(project39))

xbar39=mean(skew39,trim=0,na.rm=FALSE)

MSE39= ((xbar39)^2)/100

####INVOLVING THE MEDIAN 
skew40 <- vector("numeric", nrow(project39))  
for (i in 1:nrow(project39)){
  skew40[i] = skewness(project39[i, ], na.rm=TRUE) 
  MIN40= min(project39[i,])
  MAX40= max(project39[i,])
  MEDIAN40=median(project39[i,])
  skewness.M37=(MIN37+MAX37-2*MEDIAN37)/(MAX37-MIN37)
}
setNames(skew40, rownames(project39))

xbar40=mean(skew40,trim=0,na.rm=FALSE)

MSE40= ((xbar40)^2)/100





#####MIDRANGE FORMULAE INVOLVING THE MEAN WITH N=1000, MEAN=0 AND SD=1
set.seed(1)
project41=matrix(sample(rnorm(1000,mean=0,sd=1),100000,replace=TRUE),ncol=1000)
MIN41<-min(project41[1,])
MAX41<-max(project41[1,])
MEAN41=mean(project41[1,])
####INVOLVING THE MEAN
skew41 <- vector("numeric", nrow(project41))  
for (i in 1:nrow(project41)){
  skew41[i] = skewness(project41[i, ], na.rm=TRUE) 
  MIN41= min(project41[i,])
  MAX41= max(project41[i,])
  MEAN41=mean(project41[i,])
  skewness.M41=(MIN41+MAX41-2*MEAN41)/(MAX41-MIN41)
}
setNames(skew41, rownames(project41))

xbar41=mean(skew41,trim=0,na.rm=FALSE)

MSE41= ((xbar41)^2)/100

####INVOLVING THE MEDIAN 
skew42 <- vector("numeric", nrow(project41))  
for (i in 1:nrow(project41)){
  skew42[i] = skewness(project41[i, ], na.rm=TRUE) 
  MIN42= min(project41[i,])
  MAX42= max(project41[i,])
  MEDIAN42=median(project41[i,])
  skewness.M42=(MIN42+MAX42-2*MEDIAN42)/(MAX42-MIN42)
}
setNames(skew42, rownames(project41))

xbar42=mean(skew41,trim=0,na.rm=FALSE)

MSE42= ((xbar42)^2)/100




















#####MIDRANGE FORMULAE INVOLVING THE MEAN WITH N=10, MEAN=0 AND SD=2
set.seed(1)
project43=matrix(sample(rnorm(10,mean=0,sd=2),1000,replace=TRUE),ncol=10)
MIN43<-min(project43[1,])
MAX43<-max(project43[1,])
MEAN43=mean(project43[1,])
####INVOLVING THE MEAN
skew43<- vector("numeric", nrow(project43))  
for (i in 1:nrow(project43)){
  skew43[i] = skewness(project43[i, ], na.rm=TRUE) 
  MIN43= min(project43[i,])
  MAX43= max(project43[i,])
  MEAN43=mean(project43[i,])
  skewness.M43=(MIN43+MAX43-2*MEAN43)/(MAX43-MIN43)
}
setNames(skew43, rownames(project43))

xbar43=mean(skew43,trim=0,na.rm=FALSE)

MSE43= ((xbar43)^2)/100

####INVOLVING THE MEDIAN 
skew44 <- vector("numeric", nrow(project43))  
for (i in 1:nrow(project43)){
  skew44[i] = skewness(project43[i, ], na.rm=TRUE) 
  MIN44= min(project43[i,])
  MAX44= max(project43[i,])
  MEDIAN44=median(project43[i,])
  skewness.M44=(MIN44+MAX44-2*MEDIAN44)/(MAX44-MIN44)
}
setNames(skew44, rownames(project43))

xbar44=mean(skew44,trim=0,na.rm=FALSE)

MSE44= ((xbar44)^2)/100




#####MIDRANGE FORMULAE INVOLVING THE MEAN WITH N=100, MEAN=0 AND SD=2
set.seed(1)
project45=matrix(sample(rnorm(100,mean=0,sd=2),10000,replace=TRUE),ncol=100)
MIN45<-min(project45[1,])
MAX45<-max(project45[1,])
MEAN45=mean(project45[1,])
####INVOLVING THE MEAN
skew45 <- vector("numeric", nrow(project45))  
for (i in 1:nrow(project45)){
  skew45[i] = skewness(project45[i, ], na.rm=TRUE) 
  MIN45= min(project45[i,])
  MAX45= max(project45[i,])
  MEAN45=mean(project45[i,])
  skewness.M45=(MIN45+MAX45-2*MEAN45)/(MAX45-MIN45)
}
setNames(skew45, rownames(project45))

xbar45=mean(skew45,trim=0,na.rm=FALSE)

MSE45= ((xbar45)^2)/100

####INVOLVING THE MEDIAN 
skew46 <- vector("numeric", nrow(project45))  
for (i in 1:nrow(project45)){
  skew46[i] = skewness(project45[i, ], na.rm=TRUE) 
  MIN46= min(project45[i,])
  MAX46= max(project45[i,])
  MEDIAN46=median(project45[i,])
  skewness.M46=(MIN46+MAX46-2*MEDIAN46)/(MAX46-MIN46)
}
setNames(skew46, rownames(project45))

xbar46=mean(skew46,trim=0,na.rm=FALSE)

MSE46= ((xbar46)^2)/100





#####MIDRANGE FORMULAE INVOLVING THE MEAN WITH N=1000, MEAN=0 AND SD=2
set.seed(1)
project47=matrix(sample(rnorm(1000,mean=0,sd=2),100000,replace=TRUE),ncol=1000)
MIN47<-min(project47[1,])
MAX47<-max(project47[1,])
MEAN47=mean(project47[1,])
####INVOLVING THE MEAN
skew47 <- vector("numeric", nrow(project47))  
for (i in 1:nrow(project47)){
  skew47[i] = skewness(project47[i, ], na.rm=TRUE) 
  MIN47= min(project47[i,])
  MAX47= max(project47[i,])
  MEAN47=mean(project47[i,])
  skewness.M47=(MIN47+MAX47-2*MEAN47)/(MAX47-MIN47)
}
setNames(skew47, rownames(project47))

xbar47=mean(skew47,trim=0,na.rm=FALSE)

MSE47= ((xbar47)^2)/100

####INVOLVING THE MEDIAN 
skew48 <- vector("numeric", nrow(project47))  
for (i in 1:nrow(project47)){
  skew48[i] = skewness(project47[i, ], na.rm=TRUE) 
  MIN48= min(project47[i,])
  MAX48= max(project47[i,])
  MEDIAN48=median(project47[i,])
  skewness.M48=(MIN48+MAX48-2*MEDIAN48)/(MAX48-MIN48)
}
setNames(skew48, rownames(project47))

xbar48=mean(skew48,trim=0,na.rm=FALSE)

MSE48= ((xbar48)^2)/100










#####MIDRANGE FORMULAE INVOLVING THE MEAN WITH N=10, MEAN=0 AND SD=3
set.seed(1)
project49=matrix(sample(rnorm(10,mean=0,sd=3),1000,replace=TRUE),ncol=10)
MIN49<-min(project49[1,])
MAX49<-max(project49[1,])
MEAN49=mean(project49[1,])
####INVOLVING THE MEAN
skew49<- vector("numeric", nrow(project49))  
for (i in 1:nrow(project49)){
  skew49[i] = skewness(project49[i, ], na.rm=TRUE) 
  MIN49= min(project49[i,])
  MAX49= max(project49[i,])
  MEAN49=mean(project49[i,])
  skewness.M49=(MIN49+MAX49-2*MEAN49)/(MAX49-MIN49)
}
setNames(skew49, rownames(project49))

xbar49=mean(skew49,trim=0,na.rm=FALSE)

MSE49= ((xbar49)^2)/100

####INVOLVING THE MEDIAN 
skew50 <- vector("numeric", nrow(project49))  
for (i in 1:nrow(project49)){
  skew50[i] = skewness(project49[i, ], na.rm=TRUE) 
  MIN50= min(project49[i,])
  MAX50= max(project49[i,])
  MEDIAN50=median(project49[i,])
  skewness.M50=(MIN44+MAX49-2*MEDIAN49)/(MAX49-MIN49)
}
setNames(skew49, rownames(project49))

xbar50=mean(skew50,trim=0,na.rm=FALSE)

MSE50= ((xbar50)^2)/100




#####MIDRANGE FORMULAE INVOLVING THE MEAN WITH N=100, MEAN=0 AND SD=3
set.seed(1)
project51=matrix(sample(rnorm(100,mean=0,sd=3),10000,replace=TRUE),ncol=100)
MIN51<-min(project51[1,])
MAX51<-max(project51[1,])
MEAN51=mean(project51[1,])
####INVOLVING THE MEAN
skew51 <- vector("numeric", nrow(project51))  
for (i in 1:nrow(project51)){
  skew51[i] = skewness(project51[i, ], na.rm=TRUE) 
  MIN51= min(project51[i,])
  MAX51= max(project51[i,])
  MEAN51=mean(project51[i,])
  skewness.M51=(MIN51+MAX51-2*MEAN51)/(MAX51-MIN51)
}
setNames(skew51, rownames(project51))

xbar51=mean(skew51,trim=0,na.rm=FALSE)

MSE51= ((xbar51)^2)/100

####INVOLVING THE MEDIAN 
skew52 <- vector("numeric", nrow(project51))  
for (i in 1:nrow(project51)){
  skew52[i] = skewness(project51[i, ], na.rm=TRUE) 
  MIN52= min(project51[i,])
  MAX52= max(project51[i,])
  MEDIAN52=median(project51[i,])
  skewness.M52=(MIN52+MAX52-2*MEDIAN52)/(MAX52-MIN52)
}
setNames(skew52, rownames(project51))

xbar52=mean(skew52,trim=0,na.rm=FALSE)

MSE52= ((xbar52)^2)/100





#####MIDRANGE FORMULAE INVOLVING THE MEAN WITH N=1000, MEAN=0 AND SD=3
set.seed(1)
project53=matrix(sample(rnorm(1000,mean=0,sd=3),100000,replace=TRUE),ncol=1000)
MIN53<-min(project53[1,])
MAX53<-max(project53[1,])
MEAN53=mean(project53[1,])
####INVOLVING THE MEAN
skew53 <- vector("numeric", nrow(project53))  
for (i in 1:nrow(project53)){
  skew53[i] = skewness(project53[i, ], na.rm=TRUE) 
  MIN53= min(project53[i,])
  MAX53= max(project53[i,])
  MEAN53=mean(project53[i,])
  skewness.M53=(MIN53+MAX53-2*MEAN53)/(MAX53-MIN53)
}
setNames(skew53, rownames(project53))

xbar53=mean(skew53,trim=0,na.rm=FALSE)

MSE53= ((xbar53)^2)/100

####INVOLVING THE MEDIAN 
skew54 <- vector("numeric", nrow(project53))  
for (i in 1:nrow(project53)){
  skew54[i] = skewness(project53[i, ], na.rm=TRUE) 
  MIN54= min(project53[i,])
  MAX54= max(project53[i,])
  MEDIAN54=median(project53[i,])
  skewness.M54=(MIN54+MAX54-2*MEDIAN54)/(MAX54-MIN54)
}
setNames(skew54, rownames(project53))

xbar54=mean(skew54,trim=0,na.rm=FALSE)

MSE54= ((xbar54)^2)/100