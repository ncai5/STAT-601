install.packages("MASS");library(MASS)
x1 = rnorm(20)
x2 = rnorm(20,mean=x1,sd=.01)
y = rnorm(20,mean=3+x1+x2)
lm(y~x1+x2)$coef
#   (Intercept)          x1          x2
#   2.582064   39.971344  -38.040040
lm.ridge(y~x1+x2,lambda=1)
#                 x1        x2
#  2.6214998 0.9906773 0.8973912




setwd("~/Downloads")
data=read.csv("disc09.csv",header=T);head(data)
y=data[,4]
x1=data[,1]
x2=data[,2]
x3=data[,3]

pairs(~y+x1+x2+x3)

# SS(X1|X2)

anova(lm(y~x2+x1))

anova(lm(y~x2));anova(lm(y~x1+x2))

# SS(X3|X1,X2)
anova(lm(y~x1+x2+x3)) ; anova(lm(y~x1+x2))

# SSR dep on order of explanatory variables # different SSR comes from diff order
anova(lm(y~x1+x2+x3))
anova(lm(y~x2+x1+x3))
anova(lm(y~x3+x2+x1))

##
lm.reg=lm(y~x1+x2+x3)
predict.lm(lm.reg, newdata=data.frame(x1=20, x2=40, x3=25),
           interval = "confidence")

predict.lm(lm.reg, newdata=data.frame(x1=20, x2=40, x3=25),
           interval = "prediction")
## check assumption
## (l) ##
# install.packages("MASS");library(MASS)
# install.packages("lmtest");library(lmtest)
library(nortest)
## fit vs res ##

plot(lm.reg$fitted.values ,studres(lm.reg),xlab="Fitted values",
                  ylab="Studentized residuals",
                      main="Residual vs Fitted") 
abline(h=0);abline(h=3,lty=2);abline(h=-3,lty=2)
## res QQ ##
qqnorm(studres(lm.reg),ylab="Studentized residuals",
       ylim=c(-2,2));qqline(studres(lsfit))

## Shapiro -Wilk test
shapiro.test(studres(lm.reg))


install.packages("leaps")
library(leaps)
source("myregsub.R")
N = 1
my.regsub(data[,1:3],y=data[,4],nbest=N,method="exhaustive")

my.regsub(data[,1:3], data[,4], nbest = 4,method = "exhaustive")

##  plot for standard / studentized res. vs fitted value
index=1:length(x1)
par(mfrow=c(1,1))
plot(lm.reg$fitted, stdres(lm.reg), xlab="Fitted values",
     ylab="Internally studentized residual") 
abline(h=0, lty=2)
text(lm.reg$fitted, stdres(lm.reg), labels=index, cex=1, pos=2)
plot(lm.reg$fitted, studres(lm.reg), type = "h", xlab="Fitted values",
     ylab="Externally studentized residual")
abline(h=c(0,-3,3), lty=2)
text(lm.reg$fitted, studres(lm.reg), labels=index, cex=1, pos=2)

## leverage 
lm.reg.hats = hatvalues(lm.reg)
plot(lm.reg.hats, type = "h", ylab = "Leverage") 
text(lm.reg.hats, labels = index, cex = 1) 
abline(h=2*4/19, lty = 2) # h=2 times p / n =2 times 4 / 19

## DFFITS

lm.reg.dffits = dffits(lm.reg)
plot(lm.reg.dffits, type = "h", ylab = "DFFITS", ylim = c(-2.5,4.5)) 
text(lm.reg.dffits, labels = index, cex = 0.8, pos = 1)
abline(h = c(-1,-2*sqrt(4/19), 0, 2*sqrt(4/19), 1), lty = 2) # specify your own h 

## cook's distance
lm.reg.cooksD = cooks.distance(lm.reg)
plot(lm.reg.cooksD, type = "h", ylab="Cook's Distance",ylim=c(0,2)) 
text(lm.reg.cooksD, labels = index, cex = 1)
abline(h=qf(0.50,4,15), lty=2) #check whether D_i > f_0.5,p,n-p

## dfbetas
lm.reg.dfbetas = dfbetas(lm.reg)
plot(lm.reg.dfbetas[,1], type = "h", ylab = "DFBETAS", xlab = "Index", main = "Intercept") 
text(lm.reg.dfbetas[,1], labels = index, cex = 0.8)
abline(h=c(-1, -2/sqrt(19), 0, 2/sqrt(19), 1), lty = 2) # specify your own threshold


#eliminate 18th obs
data2=data[-18,]
