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
skewness(project7[1,])


skew7 <- vector("numeric", nrow(project7))  
for (i in 1:nrow(project7)){
  skew7[i] <- skewness(project7[i, ], na.rm=TRUE) 
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
mean(project1[i,])
median(project1[i,])
sd(project1[i,])
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

skew10 <- vector("numeric", nrow(project1))  
for (i in 1:nrow(project1)){
  skew10[i] <-3*(mean(project1[i,])-median(project1[i,]))/sd(project1[i,])
}
setNames(skew10, rownames(project1))

xbar10=mean(skew10,trim=0,na.rm=FALSE)

MSE10= ((xbar10)^2)/100




####karl Pearson with n=100, mean =0 ,sd=1
set.seed(1)
project2=matrix(sample(rnorm(100,mean=0,sd=1),10000,replace=TRUE),ncol=100)
mean(project2[i,])
median(project2[i,])
sd(project2[i,])
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

skew11 <- vector("numeric", nrow(project2))  
for (i in 1:nrow(project2)){
  skew11[i]<-3*(mean(project2[i,])-median(project2[i,]))/sd(project2[i,])
}
setNames(skew11, rownames(project2))

xbar11=mean(skew11,trim=0,na.rm=FALSE)

MSE11= ((xbar11)^2)/100






####karl Pearson with n=1000, mean =0 ,sd=1
set.seed(1)
project3=matrix(sample(rnorm(1000,mean=0,sd=1),100000,replace=TRUE),ncol=1000)
mean(project3[i,])
median(project3[i,])
sd(project3[i,])
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

skew12<- vector("numeric", nrow(project3))  
for (i in 1:nrow(project3)){
  skew12[i] <-3*(getmode(project3[i,])-median(project3[i,]))/sd(project3[i,])
}
setNames(skew12, rownames(project3))

xbar12=mean(skew12,trim=0,na.rm=FALSE)

MSE12= ((xbar12)^2)/100



####karl Pearson with n=10, mean =0 ,sd=2
set.seed(1)
project4=matrix(sample(rnorm(10,mean=0,sd=2),1000,replace=TRUE),ncol=10)
mean(project4[i,])
median(project4[i,])
sd(project4[i,])
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

skew13 <- vector("numeric", nrow(project4))  
for (i in 1:nrow(project4)){
  skew13[i] <-3*(mean(project4[i,])-median(project4[i,]))/sd(project4[i,])
}
setNames(skew13, rownames(project4))

xbar13=mean(skew13,trim=0,na.rm=FALSE)

MSE13= ((xbar13)^2)/100




####karl Pearson with n=100, mean =0 ,sd=2
set.seed(1)
project5=matrix(sample(rnorm(100,mean=0,sd=2),10000,replace=TRUE),ncol=100)
mean(project5[i,])
median(project5[i,])
sd(project5[i,])
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

skew14 <- vector("numeric", nrow(project5))  
for (i in 1:nrow(project14)){
  skew14[i] <-3*(mean(project5[i,])-median(project5[i,]))/sd(project5[i,])
}
setNames(skew14, rownames(project5))

xbar14=mean(skew14,trim=0,na.rm=FALSE)

MSE14= ((xbar14)^2)/100



####karl Pearson with n=1000, mean =0 ,sd=2
set.seed(1)
project6=matrix(sample(rnorm(1000,mean=0,sd=2),100000,replace=TRUE),ncol=1000)
mean(project6[i,])
median(project6[i,])
sd(project6[i,])
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

skew15<- vector("numeric", nrow(project6))  
for (i in 1:nrow(project6)){
  skew15[i] <-3*(getmode(project6[i,])-median(project6[i,]))/sd(project6[i,])
}
setNames(skew15, rownames(project6))

xbar15=mean(skew15,trim=0,na.rm=FALSE)

MSE15= ((xbar15)^2)/100









####karl Pearson with n=10, mean =0 ,sd=3
set.seed(1)
project7=matrix(sample(rnorm(10,mean=0,sd=3),1000,replace=TRUE),ncol=10)
mean(project7[i,])
median(project7[i,])
sd(project7[i,])
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

skew16 <- vector("numeric", nrow(project7))  
for (i in 1:nrow(project7)){
  skew16[i] <-3*(mean(project7[i,])-median(project7[i,]))/sd(project7[i,])
}
setNames(skew16, rownames(project7))

xbar16=mean(skew16,trim=0,na.rm=FALSE)

MSE16= ((xbar16)^2)/100




####karl Pearson with n=100, mean =0 ,sd=3
set.seed(1)
project8=matrix(sample(rnorm(100,mean=0,sd=3),10000,replace=TRUE),ncol=100)
mean(project8[i,])
median(project8[i,])
sd(project8[i,])
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

skew17 <- vector("numeric", nrow(project8))  
for (i in 1:nrow(project8)){
  skew17[i] <-3*(mean(project8[i,])-median(project8[i,]))/sd(project8[i,])
}
setNames(skew17, rownames(project8))

xbar17=mean(skew17,trim=0,na.rm=FALSE)

MSE17= ((xbar17)^2)/100



####karl Pearson with n=1000, mean =0 ,sd=3
set.seed(1)
project9=matrix(sample(rnorm(1000,mean=0,sd=3),100000,replace=TRUE),ncol=1000)
mean(project9[i,])
median(project9[i,])
sd(project9[i,])
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

skew18<- vector("numeric", nrow(project9))  
for (i in 1:nrow(project9)){
  skew18[i] <-3*(getmode(project9[i,])-median(project9[i,]))/sd(project9[i,])
}
setNames(skew18, rownames(project9))

xbar18=mean(skew18,trim=0,na.rm=FALSE)

MSE18= ((xbar18)^2)/100
















######BOWLEY with n=10,mean=0,sd=1
project1=matrix(sample(rnorm(10,mean=0,sd=1),1000,replace=TRUE),ncol=10)
QUANTILE19<-quantile(project1[1,],probs = c(0.25,0.5,0.75),type = 6,names = FALSE)
QUANTILE19[2]
skew19 <- vector("numeric", nrow(project1))  
for (i in 1:nrow(project1)){
  QUANTILES19<- quantile(project1[i,],probs=c(0.25,0.50,0.75),type=6,names = FALSE)
  skew19[i] <-(QUANTILE19[3]+QUANTILE19[1]-2*QUANTILE19[2])/(QUANTILE19[3]-QUANTILE19[1])
}
setNames(skew19, rownames(project1))

xbar19=mean(skew19,trim=0,na.rm=FALSE)

MSE19= ((xbar19)^2)/100




######BOWLEY with n=100,mean=0,sd=1
set.seed(1)
project2=matrix(sample(rnorm(100,mean=0,sd=1),10000,replace=TRUE),ncol=100)
QUANTILE20<-quantile(project2[1,],probs = c(0.25,0.5,0.75),type = 6,names = FALSE)
QUANTILE20[2]
skew20 <- vector("numeric", nrow(project2))  
for (i in 1:nrow(project2)){
  QUANTILES20<- quantile(project2[i,],probs=c(0.25,0.50,0.75),type=6,names = FALSE)
  skew20[i] <-(QUANTILE20[3]+QUANTILE20[1]-2*QUANTILE20[2])/(QUANTILE20[3]-QUANTILE20[1])
}
setNames(skew20, rownames(project2))

xbar20=mean(skew20,trim=0,na.rm=FALSE)

MSE20= ((xbar20)^2)/100


######BOWLEY with n=1000,mean=0,sd=1
set.seed(1)
project3=matrix(sample(rnorm(1000,mean=0,sd=1),100000,replace=TRUE),ncol=1000)
QUANTILE21<-quantile(project3[1,],probs = c(0.25,0.5,0.75),type = 6,names = FALSE)
QUANTILE21[2]
skew21 <- vector("numeric", nrow(project3))  
for (i in 1:nrow(project3)){
  QUANTILES21<- quantile(project3[i,],probs=c(0.25,0.50,0.75),type=6,names = FALSE)
  skew21[i] <-(QUANTILE21[3]+QUANTILE21[1]-2*QUANTILE21[2])/(QUANTILE21[3]-QUANTILE21[1])
}
setNames(skew21, rownames(project3))

xbar21=mean(skew21,trim=0,na.rm=FALSE)

MSE21= ((xbar21)^2)/100





######BOWLEY with n=10,mean=0,sd=2
set.seed(1)
project4=matrix(sample(rnorm(10,mean=0,sd=2),1000,replace=TRUE),ncol=10)
QUANTILE22<-quantile(project4[1,],probs = c(0.25,0.5,0.75),type = 6,names = FALSE)
QUANTILE22[2]
skew22 <- vector("numeric", nrow(project4))  
for (i in 1:nrow(project4)){
  QUANTILES22<- quantile(project4[i,],probs=c(0.25,0.50,0.75),type=6,names = FALSE)
  skew22[i] <-(QUANTILE22[3]+QUANTILE22[1]-2*QUANTILE22[2])/(QUANTILE22[3]-QUANTILE22[1])
}
setNames(skew22, rownames(project4))

xbar22=mean(skew22,trim=0,na.rm=FALSE)

MSE22= ((xbar22)^2)/100




######BOWLEY with n=100,mean=0,sd=2
set.seed(1)
project5=matrix(sample(rnorm(100,mean=0,sd=2),10000,replace=TRUE),ncol=100)
QUANTILE23<-quantile(project5[1,],probs = c(0.25,0.5,0.75),type = 6,names = FALSE)
QUANTILE23[2]
skew23 <- vector("numeric", nrow(project5))  
for (i in 1:nrow(project5)){
  QUANTILES23<- quantile(project5[i,],probs=c(0.25,0.50,0.75),type=6,names = FALSE)
  skew23[i] <-(QUANTILE23[3]+QUANTILE23[1]-2*QUANTILE23[2])/(QUANTILE23[3]-QUANTILE23[1])
}
setNames(skew23, rownames(project23))

xbar23=mean(skew23,trim=0,na.rm=FALSE)

MSE23= ((xbar23)^2)/100


######BOWLEY with n=1000,mean=0,sd=2
set.seed(1)
project6=matrix(sample(rnorm(1000,mean=0,sd=2),100000,replace=TRUE),ncol=1000)
QUANTILE24<-quantile(project6[1,],probs = c(0.25,0.5,0.75),type = 6,names = FALSE)
QUANTILE24[2]
skew24 <- vector("numeric", nrow(project6))  
for (i in 1:nrow(project6)){
  QUANTILES24<- quantile(project6[i,],probs=c(0.25,0.50,0.75),type=6,names = FALSE)
  skew24[i] <-(QUANTILE24[3]+QUANTILE24[1]-2*QUANTILE24[2])/(QUANTILE24[3]-QUANTILE24[1])
}
setNames(skew24, rownames(project6))

xbar24=mean(skew24,trim=0,na.rm=FALSE)

MSE24= ((xbar24)^2)/100





######BOWLEY with n=10,mean=0,sd=3
set.seed(1)
project7=matrix(sample(rnorm(10,mean=0,sd=3),1000,replace=TRUE),ncol=10)
QUANTILE25<-quantile(project7[1,],probs = c(0.25,0.5,0.75),type = 6,names = FALSE)
QUANTILE25[2]
skew25 <- vector("numeric", nrow(project7))  
for (i in 1:nrow(project25)){
  QUANTILES25<- quantile(project7[i,],probs=c(0.25,0.50,0.75),type=6,names = FALSE)
  skew25[i] <-(QUANTILE25[3]+QUANTILE25[1]-2*QUANTILE25[2])/(QUANTILE25[3]-QUANTILE25[1])
}
setNames(skew25, rownames(project7))

xbar25=mean(skew25,trim=0,na.rm=FALSE)

MSE25= ((xbar25)^2)/100




######BOWLEY with n=100,mean=0,sd=3
set.seed(1)
project8=matrix(sample(rnorm(100,mean=0,sd=3),10000,replace=TRUE),ncol=100)
QUANTILE26<-quantile(project8[1,],probs = c(0.25,0.5,0.75),type = 6,names = FALSE)
QUANTILE26[2]
skew26 <- vector("numeric", nrow(project8))  
for (i in 1:nrow(project8)){
  QUANTILES26<- quantile(project8[i,],probs=c(0.25,0.50,0.75),type=6,names = FALSE)
  skew26[i] <-(QUANTILE26[3]+QUANTILE26[1]-2*QUANTILE26[2])/(QUANTILE26[3]-QUANTILE26[1])
}
setNames(skew26, rownames(project8))

xbar26=mean(skew26,trim=0,na.rm=FALSE)

MSE26= ((xbar26)^2)/100


######BOWLEY with n=1000,mean=0,sd=3
set.seed(1)
project9=matrix(sample(rnorm(1000,mean=0,sd=3),100000,replace=TRUE),ncol=1000)
QUANTILE27<-quantile(project9[1,],probs = c(0.25,0.5,0.75),type = 6,names = FALSE)
QUANTILE27[2]
skew27 <- vector("numeric", nrow(project9))  
for (i in 1:nrow(project9)){
  QUANTILES27<- quantile(project9[i,],probs=c(0.25,0.50,0.75),type=6,names = FALSE)
  skew27[i] <-(QUANTILE27[3]+QUANTILE27[1]-2*QUANTILE27[2])/(QUANTILE27[3]-QUANTILE27[1])
}
setNames(skew27, rownames(project9))

xbar27=mean(skew27,trim=0,na.rm=FALSE)

A= ((xbar27)^2)/100







####### KELLY WITH N=10, MEAN = 0 AND SD=1
set.seed(1)
project1=matrix(sample(rnorm(10,mean=0,sd=1),1000,replace=TRUE),ncol=10)
DECILE28=quantile(project1[1,],probs=c(0.10,0.50,0.90),type=6,names = FALSE)
skew28 <- vector("numeric", nrow(project1))  
for (i in 1:nrow(project1)){
  DECILE28=quantile(project1[i,],probs=c(0.10,0.50,0.90),type=6,names = FALSE)
  skew28[i] <-(DECILE28[3]+DECILE28[1]-2*DECILE28[2])/(DECILE28[3]-DECILE28[1])
}
setNames(skew28, rownames(project1))

xbar28=mean(skew28,trim=0,na.rm=FALSE)

MSE28= ((xbar28)^2)/100



####### KELLY WITH N=100, MEAN = 0 AND SD=1
set.seed(1)
project2=matrix(sample(rnorm(100,mean=0,sd=1),10000,replace=TRUE),ncol=100)
DECILE29=quantile(project2[1,],probs=c(0.10,0.50,0.90),type=6,names = FALSE)
skew29 <- vector("numeric", nrow(project2))  
for (i in 1:nrow(project29)){
  skew29[i] <- skewness(project2[i, ], na.rm=TRUE) 
  DECILE29=quantile(project2[i,],probs=c(0.10,0.50,0.90),type=6,names = FALSE)
  skew29[i] <-(DECILE29[3]+DECILE29[1]-2*DECILE29[2])/(DECILE29[3]-DECILE29[1])
}
setNames(skew29, rownames(project2))

xbar29=mean(skew29,trim=0,na.rm=FALSE)

MSE29= ((xbar29)^2)/100



####### KELLY WITH N=1000, MEAN = 0 AND SD=1
set.seed(1)
project3=matrix(sample(rnorm(1000,mean=0,sd=1),100000,replace=TRUE),ncol=1000)
DECILE30=quantile(project3[1,],probs=c(0.10,0.50,0.90),type=6,names = FALSE)
skew30 <- vector("numeric", nrow(project3))  
for (i in 1:nrow(project3)){
  DECILE30=quantile(project3[i,],probs=c(0.10,0.50,0.90),type=6,names = FALSE)
  skew30[i] <-(DECILE30[3]+DECILE30[1]-2*DECILE30[2])/(DECILE30[3]-DECILE30[1])
}
setNames(skew30, rownames(project3))

xbar30=mean(skew30,trim=0,na.rm=FALSE)

MSE30= ((xbar30)^2)/100






####### KELLY WITH N=10, MEAN = 0 AND SD=2
set.seed(1)
project4=matrix(sample(rnorm(10,mean=0,sd=2),1000,replace=TRUE),ncol=10)
DECILE31=quantile(project4[1,],probs=c(0.10,0.50,0.90),type=6,names = FALSE)
skew31 <- vector("numeric", nrow(project4))  
for (i in 1:nrow(project4)){
  DECILE31=quantile(project4[i,],probs=c(0.10,0.50,0.90),type=6,names = FALSE)
  skew31[i] <-(DECILE31[3]+DECILE31[1]-2*DECILE31[2])/(DECILE31[3]-DECILE31[1])
}
setNames(skew31, rownames(project4))

xbar31=mean(skew31,trim=0,na.rm=FALSE)

MSE31= ((xbar31)^2)/100



####### KELLY WITH N=100, MEAN = 0 AND SD=2
set.seed(1)
project5=matrix(sample(rnorm(100,mean=0,sd=2),10000,replace=TRUE),ncol=100)
DECILE32=quantile(project5[1,],probs=c(0.10,0.50,0.90),type=6,names = FALSE)
skew32 <- vector("numeric", nrow(project5))  
for (i in 1:nrow(project5)){
  DECILE32=quantile(project5[i,],probs=c(0.10,0.50,0.90),type=6,names = FALSE)
  skew32[i] <-(DECILE32[3]+DECILE32[1]-2*DECILE32[2])/(DECILE32[3]-DECILE32[1])
}
setNames(skew32, rownames(project5))

xbar32=mean(skew32,trim=0,na.rm=FALSE)

MSE32= ((xbar32)^2)/100



####### KELLY WITH N=1000, MEAN = 0 AND SD=2
set.seed(1)
project6=matrix(sample(rnorm(1000,mean=0,sd=2),100000,replace=TRUE),ncol=1000)
DECILE33=quantile(project6[1,],probs=c(0.10,0.50,0.90),type=6,names = FALSE)
skew33 <- vector("numeric", nrow(project6))  
for (i in 1:nrow(project6)){
  DECILE33=quantile(project6[i,],probs=c(0.10,0.50,0.90),type=6,names = FALSE)
  skew33[i] <-(DECILE33[3]+DECILE33[1]-2*DECILE33[2])/(DECILE33[3]-DECILE33[1])
}
setNames(skew33, rownames(project6))

xbar33=mean(skew33,trim=0,na.rm=FALSE)

MSE33= ((xbar33)^2)/100





####### KELLY WITH N=10, MEAN = 0 AND SD=3
set.seed(1)
project7=matrix(sample(rnorm(10,mean=0,sd=3),1000,replace=TRUE),ncol=10)
DECILE34=quantile(project7[1,],probs=c(0.10,0.50,0.90),type=6,names = FALSE)
skew34 <- vector("numeric", nrow(project7))  
for (i in 1:nrow(project7)){
  DECILE34=quantile(project7[i,],probs=c(0.10,0.50,0.90),type=6,names = FALSE)
  skew34[i] <-(DECILE34[3]+DECILE34[1]-2*DECILE34[2])/(DECILE34[3]-DECILE34[1])
}
setNames(skew34, rownames(project7))

xbar34=mean(skew34,trim=0,na.rm=FALSE)

MSE34= ((xbar34)^2)/100



####### KELLY WITH N=100, MEAN = 0 AND SD=3
set.seed(1)
project8=matrix(sample(rnorm(100,mean=0,sd=3),10000,replace=TRUE),ncol=100)
DECILE35=quantile(project8[1,],probs=c(0.10,0.50,0.90),type=6,names = FALSE)
skew35 <- vector("numeric", nrow(project8))  
for (i in 1:nrow(project8)){
  DECILE35=quantile(project8[i,],probs=c(0.10,0.50,0.90),type=6,names = FALSE)
  skew35[i] <-(DECILE35[3]+DECILE35[1]-2*DECILE35[2])/(DECILE35[3]-DECILE35[1])
}
setNames(skew35, rownames(project8))

xbar35=mean(skew35,trim=0,na.rm=FALSE)

MSE35= ((xbar35)^2)/100



####### KELLY WITH N=1000, MEAN = 0 AND SD=3
set.seed(1)
project9=matrix(sample(rnorm(1000,mean=0,sd=3),100000,replace=TRUE),ncol=1000)
DECILE36=quantile(project9[1,],probs=c(0.10,0.50,0.90),type=6,names = FALSE)
skew36 <- vector("numeric", nrow(project9))  
for (i in 1:nrow(project9)){
  DECILE36=quantile(project9[i,],probs=c(0.10,0.50,0.90),type=6,names = FALSE)
  skew36[i] <-(DECILE36[3]+DECILE36[1]-2*DECILE36[2])/(DECILE36[3]-DECILE36[1])
}
setNames(skew36, rownames(project9))

xbar36=mean(skew36,trim=0,na.rm=FALSE)

MSE36= ((xbar36)^2)/100








#####MIDRANGE FORMULAE INVOLVING THE MEAN WITH N=10, MEAN=0 AND SD=1
set.seed(1)
project1=matrix(sample(rnorm(10,mean=0,sd=1),1000,replace=TRUE),ncol=10)
MIN37<-min(project1[1,])
MAX37<-max(project1[1,])
MEAN37=mean(project1[1,])
####INVOLVING THE MEAN
skew37 <- vector("numeric", nrow(project1))  
for (i in 1:nrow(project1)){
  MIN37= min(project1[i,])
  MAX37= max(project1[i,])
  MEAN37=mean(project1[i,])
  skew37[i]=(MIN37+MAX37-2*MEAN37)/(MAX37-MIN37)
}
setNames(skew37, rownames(project1))

xbar37=mean(skew37,trim=0,na.rm=FALSE)

MSE37= ((xbar37)^2)/100

####INVOLVING THE MEDIAN 
skew38 <- vector("numeric", nrow(project1))  
for (i in 1:nrow(project1)){
  MIN37= min(project1[i,])
  MAX37= max(project1[i,])
  MEDIAN37=median(project1[i,])
  skew38[i]=(MIN37+MAX37-2*MEDIAN37)/(MAX37-MIN37)
}
setNames(skew38, rownames(project1))

xbar38=mean(skew38,trim=0,na.rm=FALSE)

MSE38= ((xbar38)^2)/100




#####MIDRANGE FORMULAE INVOLVING THE MEAN WITH N=100, MEAN=0 AND SD=1
set.seed(1)
project2=matrix(sample(rnorm(100,mean=0,sd=1),10000,replace=TRUE),ncol=100)
MIN39<-min(project2[1,])
MAX39<-max(project2[1,])
MEAN39=mean(project2[1,])
####INVOLVING THE MEAN
skew39 <- vector("numeric", nrow(project2))  
for (i in 1:nrow(project2)){
  MIN39= min(project2[i,])
  MAX39= max(project2[i,])
  MEAN39=mean(project2[i,])
  skew39[i]=(MIN39+MAX39-2*MEAN39)/(MAX39-MIN39)
}
setNames(skew39, rownames(project2))

xbar39=mean(skew39,trim=0,na.rm=FALSE)

MSE39= ((xbar39)^2)/100

####INVOLVING THE MEDIAN 
skew40 <- vector("numeric", nrow(project2))  
for (i in 1:nrow(project2)){
  MIN40= min(project2[i,])
  MAX40= max(project2[i,])
  MEDIAN40=median(project2[i,])
  skew40[i]=(MIN40+MAX40-2*MEDIAN40)/(MAX40-MIN40)
}
setNames(skew40, rownames(project2))

xbar40=mean(skew40,trim=0,na.rm=FALSE)

MSE40= ((xbar40)^2)/100





#####MIDRANGE FORMULAE INVOLVING THE MEAN WITH N=1000, MEAN=0 AND SD=1
set.seed(1)
project3=matrix(sample(rnorm(1000,mean=0,sd=1),100000,replace=TRUE),ncol=1000)
MIN41<-min(project3[1,])
MAX41<-max(project3[1,])
MEAN41=mean(project3[1,])
####INVOLVING THE MEAN
skew41 <- vector("numeric", nrow(project3))  
for (i in 1:nrow(project3)){
  MIN41= min(project3[i,])
  MAX41= max(project3[i,])
  MEAN41=mean(project3[i,])
  skew41[i]=(MIN41+MAX41-2*MEAN41)/(MAX41-MIN41)
}
setNames(skew41, rownames(project3))

xbar41=mean(skew41,trim=0,na.rm=FALSE)

MSE41= ((xbar41)^2)/100

####INVOLVING THE MEDIAN 
skew42 <- vector("numeric", nrow(project3))  
for (i in 1:nrow(project3)){
  MIN42= min(project3[i,])
  MAX42= max(project3[i,])
  MEDIAN42=median(project3[i,])
  skew42[i]=(MIN42+MAX42-2*MEDIAN42)/(MAX42-MIN42)
}
setNames(skew42, rownames(project3))

xbar42=mean(skew42,trim=0,na.rm=FALSE)

MSE42= ((xbar42)^2)/100




















#####MIDRANGE FORMULAE INVOLVING THE MEAN WITH N=10, MEAN=0 AND SD=2
set.seed(1)
project4=matrix(sample(rnorm(10,mean=0,sd=2),1000,replace=TRUE),ncol=10)
MIN43<-min(project4[1,])
MAX43<-max(project4[1,])
MEAN43=mean(project4[1,])
####INVOLVING THE MEAN
skew43<- vector("numeric", nrow(project4))  
for (i in 1:nrow(project4)){
  MIN43= min(project4[i,])
  MAX43= max(project4[i,])
  MEAN43=mean(project4[i,])
  skew43[i]=(MIN43+MAX43-2*MEAN43)/(MAX43-MIN43)
}
setNames(skew43, rownames(project4))

xbar43=mean(skew43,trim=0,na.rm=FALSE)

MSE43= ((xbar43)^2)/100

####INVOLVING THE MEDIAN 
skew44 <- vector("numeric", nrow(project4))  
for (i in 1:nrow(project4)){
  MIN44= min(project4[i,])
  MAX44= max(project4[i,])
  MEDIAN44=median(project4[i,])
  skew44[i]=(MIN44+MAX44-2*MEDIAN44)/(MAX44-MIN44)
}
setNames(skew44, rownames(project4))

xbar44=mean(skew44,trim=0,na.rm=FALSE)

MSE44= ((xbar44)^2)/100




#####MIDRANGE FORMULAE INVOLVING THE MEAN WITH N=100, MEAN=0 AND SD=2
set.seed(1)
project5=matrix(sample(rnorm(100,mean=0,sd=2),10000,replace=TRUE),ncol=100)
MIN45<-min(project5[1,])
MAX45<-max(project5[1,])
MEAN45=mean(project5[1,])
####INVOLVING THE MEAN
skew45 <- vector("numeric", nrow(project5))  
for (i in 1:nrow(project5)){
  MIN45= min(project5[i,])
  MAX45= max(project5[i,])
  MEAN45=mean(project5[i,])
  skew45[i]=(MIN45+MAX45-2*MEAN45)/(MAX45-MIN45)
}
setNames(skew45, rownames(project5))

xbar45=mean(skew45,trim=0,na.rm=FALSE)

MSE45= ((xbar45)^2)/100

####INVOLVING THE MEDIAN 
skew46 <- vector("numeric", nrow(project5))  
for (i in 1:nrow(project5)){
  MIN46= min(project5[i,])
  MAX46= max(project5[i,])
  MEDIAN46=median(project5[i,])
  skew46[i]=(MIN46+MAX46-2*MEDIAN46)/(MAX46-MIN46)
}
setNames(skew46, rownames(project5))

xbar46=mean(skew46,trim=0,na.rm=FALSE)

MSE46= ((xbar46)^2)/100





#####MIDRANGE FORMULAE INVOLVING THE MEAN WITH N=1000, MEAN=0 AND SD=2
set.seed(1)
project6=matrix(sample(rnorm(1000,mean=0,sd=2),100000,replace=TRUE),ncol=1000)
MIN47<-min(project6[1,])
MAX47<-max(project6[1,])
MEAN47=mean(project6[1,])
####INVOLVING THE MEAN
skew47 <- vector("numeric", nrow(project6))  
for (i in 1:nrow(project6)){
  MIN47= min(project6[i,])
  MAX47= max(project6[i,])
  MEAN47=mean(project6[i,])
  skew47[i]=(MIN47+MAX47-2*MEAN47)/(MAX47-MIN47)
}
setNames(skew47, rownames(project6))

xbar47=mean(skew47,trim=0,na.rm=FALSE)

MSE47= ((xbar47)^2)/100

####INVOLVING THE MEDIAN 
skew48 <- vector("numeric", nrow(project6))  
for (i in 1:nrow(project6)){
  MIN48= min(project6[i,])
  MAX48= max(project6[i,])
  MEDIAN48=median(project6[i,])
  skew48[i]=(MIN48+MAX48-2*MEDIAN48)/(MAX48-MIN48)
}
setNames(skew48, rownames(project6))

xbar48=mean(skew48,trim=0,na.rm=FALSE)

MSE48= ((xbar48)^2)/100










#####MIDRANGE FORMULAE INVOLVING THE MEAN WITH N=10, MEAN=0 AND SD=3
set.seed(1)
project7=matrix(sample(rnorm(10,mean=0,sd=3),1000,replace=TRUE),ncol=10)
MIN49<-min(project7[1,])
MAX49<-max(project7[1,])
MEAN49=mean(project7[1,])
####INVOLVING THE MEAN
skew49<- vector("numeric", nrow(project7))  
for (i in 1:nrow(project7)){
  MIN49= min(project7[i,])
  MAX49= max(project7[i,])
  MEAN49=mean(project7[i,])
  skew49[i]=(MIN49+MAX49-2*MEAN49)/(MAX49-MIN49)
}
setNames(skew49, rownames(project7))

xbar49=mean(skew49,trim=0,na.rm=FALSE)

MSE49= ((xbar49)^2)/100

####INVOLVING THE MEDIAN 
skew50 <- vector("numeric", nrow(project7))  
for (i in 1:nrow(project7)){
  MIN50= min(project7[i,])
  MAX50= max(project7[i,])
  MEDIAN50=median(project7[i,])
  skew50[i]=(MIN50+MAX50-2*MEDIAN50)/(MAX50-MIN50)
}
setNames(skew50, rownames(project7))

xbar50=mean(skew50,trim=0,na.rm=FALSE)

MSE50= ((xbar50)^2)/100




#####MIDRANGE FORMULAE INVOLVING THE MEAN WITH N=100, MEAN=0 AND SD=3
set.seed(1)
project8=matrix(sample(rnorm(100,mean=0,sd=3),10000,replace=TRUE),ncol=100)
MIN51<-min(project8[1,])
MAX51<-max(project8[1,])
MEAN51=mean(project8[1,])
####INVOLVING THE MEAN
skew51 <- vector("numeric", nrow(project8))  
for (i in 1:nrow(project8)){
  MIN51= min(project8[i,])
  MAX51= max(project8[i,])
  MEAN51=mean(project8[i,])
  skew51[i]=(MIN51+MAX51-2*MEAN51)/(MAX51-MIN51)
}
setNames(skew51, rownames(project8))

xbar51=mean(skew51,trim=0,na.rm=FALSE)

MSE51= ((xbar51)^2)/100

####INVOLVING THE MEDIAN 
skew52 <- vector("numeric", nrow(project8))  
for (i in 1:nrow(project8)){
  MIN52= min(project8[i,])
  MAX52= max(project8[i,])
  MEDIAN52=median(project8[i,])
  skew52[i]=(MIN52+MAX52-2*MEDIAN52)/(MAX52-MIN52)
}
setNames(skew52, rownames(project8))

xbar52=mean(skew52,trim=0,na.rm=FALSE)

MSE52= ((xbar52)^2)/100





#####MIDRANGE FORMULAE INVOLVING THE MEAN WITH N=1000, MEAN=0 AND SD=3
set.seed(1)
project9=matrix(sample(rnorm(1000,mean=0,sd=3),100000,replace=TRUE),ncol=1000)
MIN53<-min(project9[1,])
MAX53<-max(project9[1,])
MEAN53=mean(project9[1,])
####INVOLVING THE MEAN
skew53 <- vector("numeric", nrow(project9))  
for (i in 1:nrow(project9)){
  MIN53= min(project9[i,])
  MAX53= max(project9[i,])
  MEAN53=mean(project9[i,])
  skew53[i]=(MIN53+MAX53-2*MEAN53)/(MAX53-MIN53)
}
setNames(skew53, rownames(project9))

xbar53=mean(skew53,trim=0,na.rm=FALSE)

MSE53= ((xbar53)^2)/100

####INVOLVING THE MEDIAN 
skew54 <- vector("numeric", nrow(project9))  
for (i in 1:nrow(project9)){
  MIN54= min(project9[i,])
  MAX54= max(project9[i,])
  MEDIAN54=median(project9[i,])
  skew54[i]=(MIN54+MAX54-2*MEDIAN54)/(MAX54-MIN54)
}
setNames(skew54, rownames(project9))

xbar54=mean(skew54,trim=0,na.rm=FALSE)

MSE54= ((xbar54)^2)/100