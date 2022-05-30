
??skewness
library(moments)
install.packages("moments")
library(moments)
Project<-rnorm(100)
set.seed(1)
Project1<-rnorm(100)
set.seed(1)
skewness(Project)
skewness(Project1)
hist(Project)
lines(Project,col="pink",lwd=2)
mean(Project)

####Karl Pearson's coefficient of skewness###
install.packages("rlang", dependencies = TRUE)
install.packages("EnvStats",dependencies = TRUE)
library(EnvStats)
install.packages("ggplot2",dependencies = TRUE)
library(EnvStats)
