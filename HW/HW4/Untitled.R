1.
setwd("/Users/chenjiawen/Desktop")
patient=read.table("patient.txt",header=T)
y=patient$y
x1=patient$x1;x2=patient$x2;x3=patient$x3;x4=patient$x4
pairs(~y+x1+x2+x3)
cor(patient)
lm.reg=lm(y~x1+x2+x3)
summary(lm.reg)

anova(lm(y~x1+x2+x3)) ; anova(lm(y~x1+x3))
qf(0.95,1,42)
qt(0.025,42)

c(-0.4420-2.0181*0.4920,-0.4420+2.0181*0.4920)
predict.lm(lm.reg, newdata=data.frame(x1=35, x2=45, x3=2.2),
           interval = "confidence",level=0.95)

xh=matrix(data=c(1,35,45,2.2),ncol=1)
beta_hat=matrix(data=c(158.4913,-1.1416,-0.4420,-13.4702),ncol=1)
x=matrix(data=c(rep(1,nrow(patient)),x1,x2,x3),nrow=nrow(patient))
c(t(xh)%*%beta_hat+qt(0.025,42)*10.06*sqrt(t(xh)%*%solve((t(x)%*%x))%*%xh),
  t(xh)%*%beta_hat-qt(0.025,42)*10.06*sqrt(t(xh)%*%solve((t(x)%*%x))%*%xh))

c(t(xh)%*%beta_hat+qt(0.025,42)*10.06*sqrt(1+t(xh)%*%solve((t(x)%*%x))%*%xh),
  t(xh)%*%beta_hat-qt(0.025,42)*10.06*sqrt(1+t(xh)%*%solve((t(x)%*%x))%*%xh))

plot(lm.reg$fitted.values ,studres(lm.reg),xlab="Fitted values",
     ylab="Studentized residuals",
     main="Residual vs Fitted") 
abline(h=0)

qqnorm(studres(lm.reg),ylab="Studentized residuals",ylim=c(-2,2))
qqline(studres(lm.reg))
shapiro.test(studres(lm.reg))

2.         
lm.reg=lm(y~x2)
summary(lm.reg)
qt(0.025,44)
c(-2.4093+(-2.0153)*0.4806,-2.4093-(-2.0153)*0.4806)

anova(lm(y~x1));anova(lm(y~x1+x3))
anova(lm(y~x2));anova(lm(y~x2+x3))
anova(lm(y~x3))

library(leaps)
source("myregsub.R")

N = 1
my.regsub(patient[,2:4],y=patient[,1],nbest=N,method="exhaustive")

N = 4
my.regsub(patient[,2:4],y=patient[,1],nbest=N,method="exhaustive")

lm1<-lm(y~.,data=patient)
summary(lm1)
step(lm1,direction = "backward",steps = 1000)
step(lm1,direction = "backward",k=log(46),steps = 1000)

betalamda=function(model,lamda){
  y=model$model[,1]
  x=as.matrix(rep(1,nrow(model$model)),model$model[,2:ncol(model$model)])
  betahat=solve(t(x)%*%x+diag(lamda))%*%t(x)%*%y
  return(betahat)
}

4.
drink=read.table("softdrink.txt",header=T)
y=drink$y
x1=drink$x1;x2=drink$x2
pairs(~y+x1+x2)
cor(drink)
lm.reg=lm(y~x1+x2)
summary(lm.reg)
anova(lm.reg)
index=1:length(x1)
par(mfrow=c(1,1))
plot(lm.reg$fitted, stdres(lm.reg), xlab="Fitted values",
     ylab="Internally studentized residual") 
abline(h=0, lty=2)
text(lm.reg$fitted, stdres(lm.reg), labels=index, cex=0.5, pos=2)
plot(lm.reg$fitted, studres(lm.reg), type = "h", xlab="Fitted values",
     ylab="Externally studentized residual")
abline(h=c(0,-3,3), lty=2)
text(lm.reg$fitted, studres(lm.reg), labels=index, cex=0.5, pos=2)


## leverage 
lm.reg.hats = hatvalues(lm.reg)
plot(lm.reg.hats, type = "h", ylab = "Leverage") 
text(lm.reg.hats, labels = index, cex = 0.5) 
abline(h=2*3/25, lty = 2) # h=2 times p / n =2 times 4 / 19

## DFFITS

lm.reg.dffits = dffits(lm.reg)
plot(lm.reg.dffits, type = "h", ylab = "DFFITS", ylim = c(-2.5,4.5)) 
text(lm.reg.dffits, labels = index, cex = 0.5, pos = 1)
abline(h = c(-1,-2*sqrt(3/25), 0, 2*sqrt(3/25), 1), lty = 2) # specify your own h 

## cook's distance
lm.reg.cooksD = cooks.distance(lm.reg)
plot(lm.reg.cooksD, type = "h", ylab="Cook's Distance",ylim=c(0,1.5)) 
text(lm.reg.cooksD, labels = index, cex = 0.5)
abline(h=qf(0.50,3,25), lty=2) #check whether D_i > f_0.5,p,n-p

## dfbetas
lm.reg.dfbetas = dfbetas(lm.reg)
plot(lm.reg.dfbetas[,1], type = "h", ylab = "DFBETAS", xlab = "Index", main = "Intercept") 
text(lm.reg.dfbetas[,1], labels = index, cex = 0.5)
abline(h=c(-1, -2/sqrt(25), 0, 2/sqrt(25), 1), lty = 2) # specify your own threshold

drink2=drink[-9,]
lm.reg2=lm(y~x1+x2,data=drink2)
summary(lm.reg2)

