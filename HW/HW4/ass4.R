install.packages("MASS");library(MASS)
#1(a)
a=read.table("patient.txt",header = TRUE)
a

pairs(a,main="Scatter Plot Matrix")
cor(a$y,a$x1)
cor(a$y,a$x2)
cor(a$y,a$x3)
cor(a$x1,a$x2)
cor(a$x1,a$x3)
cor(a$x2,a$x3)

#1(h)(g)
lm.reg=lm(data=a, y~x1+x2+x3)
predict.lm(lm.reg, newdata=data.frame(x1=35, x2=45, x3=2.2),
           interval = "confidence")
predict.lm(lm.reg, newdata=data.frame(x1=35, x2=45, x3=2.2),
           interval = "prediction")
#1(c)
lm.reg=lm(data=a, y~x1+x2+x3)
summary(lm.reg)
confint(lm.reg)

qchisq(0.025,42)
qchisq(0.975,42)
42*(10.06^2)/qchisq(0.025,42)
42*(10.06^2)/qchisq(0.975,42)


#1(d)
a=read.table("patient.txt",header = TRUE)
lm.reg=lm(data=a, y~x1+x2+x3)
summary(lm.reg)

#1(f)
## for testing H0: beta1=beta2=beta3=0
ls.fit0=lm(data=a,y~1)
ls.fit=lm(data=a,y~x1+x2+x3)
anova(ls.fit0,ls.fit)
# cheking correlation betw vars
pairs(data=a,~y+x1+x2+x3)
lm(data=a,x1~x2+x3)

## check assumption
## (l) ##
# install.packages("MASS");library(MASS)
# install.packages("lmtest");library(lmtest)
install.packages("MASS")
library(MASS)
install.packages("lmtest")
install.packages("nortest")
library(nortest)
library(lmtest)
library(base)
## fit vs res ##

plot(lm.reg$fitted.values ,studres(lm.reg),xlab="Fitted values",
     ylab="Studentized residuals",
     main="Residual vs Fitted",cex.lab=1.5,cex.main=1.5) 
abline(h=0);abline(h=3,lty=2);abline(h=-3,lty=2)

## res QQ ##
qqnorm(studres(lm.reg),ylab="Studentized residuals",
       ylim=c(-2,2),cex.lab=1.5,cex.main=1.8)
qqline(studres(lm.reg))

## Shapiro -Wilk test
shapiro.test(studres(lm.reg))

#2(a)
setwd("/Users/cainaiqing/Desktop/601/hw4")
a=read.table("patient.txt",header = TRUE)
m=lm(a$y~a$x2)
m
summary(m)
confint(m)
t.test()

qt(0.025,44)
qt(0.975,44)

#2(b)
install.packages("car")
library(car)
a=read.table("patient.txt",header = TRUE)
lm.reg=lm(data=a, y~x1+x2+x3)
vif(lm.reg)


#2(c)
# SSR(X1) & SSR(X1|X3)
anova(lm(data=a,y~x1))
anova(lm(data=a,y~x3+x1))
# SS(X1|X2)

anova(lm(data=a,y~x2+x1))

anova(lm(data=a,y~x2));
anova(lm(data=a,y~x1+x2))

# SS(X3|X1,X2)
anova(lm(data=a,y~x1+x2+x3)) ; anova(lm(data=a,y~x1+x2))

# SSR dep on order of explanatory variables # different SSR comes from diff order
anova(lm(data=a,y~x1+x2+x3))
anova(lm(data=a,y~x2+x1+x3))
anova(lm(data=a,y~x3+x2+x1))


#2(d)
install.packages("leaps")
library(leaps)
my.regsub <- function(matrix,y,nbest,method,nvmax=8){
  temp <- regsubsets(matrix,y,nbest=nbest,method=method,nvmax=nvmax)
  temp.mat <- cbind(summary(temp)$which,
                    summary(temp)$rsq,summary(temp)$rss,
                    summary(temp)$adjr2,summary(temp)$cp,
                    summary(temp)$bic)
  dimnames(temp.mat)[[2]] <- c(dimnames(summary(temp)$which)[[2]],
                               "rsq", "rss", "adjr2", "cp", "bic")
  return(temp.mat)
}

my.regsub(a[,2:4],y=a[,1],nbest=1,method="exhaustive")

my.regsub(a[,2:4],y=a[,1], nbest = 4,method = "exhaustive")

#2(f)
fit=lm(data=a,y~x1+x2+x3)
step(fit,direction = "backward",trace=1)
fitm=lm(data=a,y~x1+x3)
summary(fitm)

#2(g)
n=46
fit=lm(data=a,y~x1+x2+x3)
step(fit,direction = "backward",trace=1,k=log(n))
fitm=lm(data=a,y~x1+x3)
summary(fitm)

#4(a)
b=read.table("softdrink.txt",header = TRUE)
b
pairs(b,main="Scatter Plot Matrix")
cor(b$y,b$x1)
cor(b$y,b$x2)
cor(b$x1,b$x2)
4(b)
lm.reg=lm(b$y~b$x1+b$x2)
summary(lm.reg)

#4(d)
index=1:length(b$x1)
par(mfrow=c(1,1))
plot(data=b,lm.reg$fitted, stdres(lm.reg), xlab="Fitted values",
     ylab="Internally studentized residual",cex.lab=1.5) 
abline(h=0, lty=2)
text(data=b,lm.reg$fitted, stdres(lm.reg), labels=index, cex=1, pos=2)
plot(data=b,lm.reg$fitted, studres(lm.reg), type = "h", xlab="Fitted values",
     ylab="Externally studentized residual",cex.lab=1.5)
abline(h=c(0,-3,3), lty=2)
text(data=b,lm.reg$fitted, studres(lm.reg), labels=index, cex=1, pos=2)

#4(e)
## leverage 
lm.reg.hats = hatvalues(data=b,lm.reg)
plot(lm.reg.hats, type = "h", ylab = "Leverage",cex.lab=1.5) 
text(lm.reg.hats, labels = index, cex = 1) 
abline(h=2*4/19, lty = 2) # h=2 times p / n =2 times 4 / 19

## DFFITS

lm.reg.dffits = dffits(lm.reg)
plot(lm.reg.dffits, type = "h", ylab = "DFFITS", ylim = c(-2.5,4.5),cex.lab=1.5) 
text(lm.reg.dffits, labels = index, cex = 0.8, pos = 1)
abline(h = c(-1,-2*sqrt(4/19), 0, 2*sqrt(4/19), 1), lty = 2) # specify your own h 

## cook's distance
lm.reg.cooksD = cooks.distance(lm.reg)
plot(lm.reg.cooksD, type = "h", ylab="Cook's Distance",ylim=c(0,2),cex.lab=1.5) 
text(lm.reg.cooksD, labels = index, cex = 1)
abline(h=qf(0.50,4,15), lty=2) #check whether D_i > f_0.5,p,n-p

## dfbetas
lm.reg.dfbetas = dfbetas(lm.reg)
plot(lm.reg.dfbetas[,1], type = "h", ylab = "DFBETAS", xlab = "Index", main = "Intercept",cex.lab=1.5,cex.main=1.5) 
text(lm.reg.dfbetas[,1], labels = index, cex = 0.8)
abline(h=c(-1, -2/sqrt(19), 0, 2/sqrt(19), 1), lty = 2) # specify your own threshold

#4(f)
b_new=read.table("softdrink_new.txt",header = TRUE)
lm.reg=lm(data=b_new,y~x1+x2)
summary(lm.reg)

#
betalamda=function(model,lamda){
  y=model$model[,1] 
  x=as.matrix(rep(1,nrow(model$model)),model$model[,2:ncol(model$model)]) 
  betahat=solve(t(x)%*%x+diag(lamda))%*%t(x)%*%y
  return(betahat)
}

penalize <- function(model,lambda) {
  if (length(model$coefficients) != length(lambda)) {
    return("lambda should have the same parameter length as the model coefficients!")
  }
  x_l <- rbind(model.matrix(model),diag(lambda))
  y_l <- c(as.vector(model$residuals+model$fitted.values),rep(0,length(lambda)))
  beta_l <- solve(t(x_l)%*%x_l)%*%t(x_l)%*%y_l
  return(beta_l)
}

penalize <- function(model,lambda) {
  if (length(model$coefficients) != length(lambda)) {
    return("lambda should have the same parameter length as the model coefficients!")
  }
  x<- rbind(model.matrix(model),diag(lambda))
  y<- c(as.vector(model$residuals+model$fitted.values),rep(0,length(lambda)))
  betalambda<- solve(t(x)%*%x)%*%t(x)%*%y
  return(betalambda)
}

rle(c(1,1,2,2,2,3,4))
rle(c(1,2,2,2,1,3,4))
