###############################################################################################
# STAT 601 Discussion 02
# Code for one sample t-tests
###############################################################################################
rm(list = ls(all = TRUE))
# This code is for "Example 1" in discussion 02 handout.
data <- read.csv('piano.csv', header=T)
attach(data)
stem(Reasoning)
mean(Reasoning)
sd(Reasoning)
sd(Reasoning)/sqrt(length(Reasoning))
t.test(Reasoning, mu=1, alternative="two.sided",conf.level=0.95)

# This code is for "Example 2" in discussion 02 handout.
left  <- c(37, 40, 11,  29,  99,  71,  6, 31)
right <- c(41, 44, 21, 105, 108, 170, 15, 29)

diff=left-right
stem(left-right)
hist(left-right)
boxplot(left-right)

par(mfrow=c(1,1))
qqnorm(diff)  # check normality assumption
qqline(diff)


t.test(left-right)
t.test(left, right, paired=T)
t.test(left, right, paired=T, alternative = "two.sided")


# This code is for "Example 3" in discussion 02 handout.
zinc = read.csv("zinc.csv")

bottom = zinc[,2]
surface = zinc[,3]
t.test(bottom, surface, paired=T)
t.test(bottom, surface, paired=T, alternative = "greater")
t.test(bottom, surface, paired=T,conf.level = 0.8)



# simulation 

S=100 # the number of simulations
n=10 # sample size for each simulation
set.seed(2017)
sample_mean=rep(NA,S)

for (i in 1:S){
  sample_norm=rnorm(n,2,2) # draw n rnadom observtions from N(2,4)
  sample_mean[i]=mean(sample_norm)
}

hist(sample_mean, xlim=c(0,4), ylim=c(0,40),xlab="sample mean",
     col=rgb(0,0,1,1/4), main="random sample(n=10) from N(2,4)", 
     breaks = seq(0,4,0.2))


# want to repeat this procedure for different sample sizes. 
S = 100
par(mfrow = c(1,3)) # plot 4 figures 
for(n in c(10,20,30)){
  sample_mean=rep(NA,S)
  for(i in 1:S){
    sample_norm=rnorm(n,2,2)
    sample_mean[i]=mean(sample_norm)
  }
  if (n == 10){
    hist(sample_mean, xlim=c(0,4), ylim=c(0,30),xlab="sample mean",
         col=rgb(0,0,1,1/4), main="random sample(n=10) from N(2,4)", 
         breaks = seq(0,4,0.2))
  }
  if (n == 20){
    hist(sample_mean, xlim=c(0,4), ylim=c(0,30),xlab="sample mean",
         col=rgb(0,1,1,1/4), main="random sample(n=20) from N(2,4)", 
         breaks = seq(0,4,0.2))
  }
  if (n == 30){
    hist(sample_mean, xlim=c(0,4), ylim=c(0,30),xlab="sample mean",
         col=rgb(0,1/4,1,1/4), main="random sample(n=30) from N(2,4)", 
         breaks = seq(0,4,0.2))
  }
}

# want to find minmum of n satisfying P[-2-n*0.3 < Z < 2-n*0.3] <= 0.2
pnorm(0,0,1) # P(Z<0)
pnorm(2,0,1) # P(Z<2)
x = 1:5
for (val in x) {
  if (val == 3){
    break
  }
  print(val)
}





for(n in 1:200){
  prob=pnorm(2-n*0.3,0,1)-pnorm(-2-n*0.3,0,1)
  if(prob <= 0.2){
    break
  }
  print(n)
}
n=9
pnorm(2-n*0.3,0,1)-pnorm(-2-n*0.3,0,1)

n=10
pnorm(2-n*0.3,0,1)-pnorm(-2-n*0.3,0,1)

