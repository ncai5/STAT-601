# 4(b)
S=100 # the number of simulations
n=10
set.seed(2017)
b0=rep(NA,S)
b1=rep(NA,S)
MSe=rep(NA,S)
for (i in 1:S){
  x=c(2, 4, 6, 8, 10, 12, 14, 16, 18, 20)
  error=rnorm(n,0,3)
  y=10+x*4+error
  
  lsfit=lm(y ~ x)
  b0[i]=lsfit$coefficients[1] 
  b1[i]=lsfit$coefficients[2]
  b0[i];b1[i];summary(lsfit);
  Sxx=sum((x-mean(x))^2)
  Sxy=sum((x-mean(x))*(y-mean(y))) 
  e=lsfit$residuals
  SSe=sum(e^2)
  MSe[i]=SSe/(n-2) 
  varb1=MSe[i]/Sxx
  varb0=MSe[i]*(1/n+mean(x)^2/Sxx) 
  
  confint(lsfit) 
}
# make graphs
par(mfrow=c(1,3))
hist(b0, ylim=c(0,70),xlab="Beta 0",col=rgb(0,0,1,1/4), main="Histogram for beta 0",breaks = seq(-15,45,5))
hist(b1, ylim=c(0,50),xlab="Beta 1",col=rgb(0,0,1,1/4), main="Histogram for beta 1")
hist(MSe, xlab="Residual Mean Square",col=rgb(0,0,1,1/4), main="Histogram for residual mean square")
summary(lsfit)

# 4(c)
S=100 # the number of simulations
n=10
set.seed(2017)
b0=rep(NA,S)
b1=rep(NA,S)
MSe=rep(NA,S)
for (i in 1:S){
  x=c(2, 6, 10, 14, 18, 22, 26, 30, 34, 38)
  error=rnorm(n,0,3)
  y=10+x*4+error
  
  lsfit=lm(y ~ x)
  b0[i]=lsfit$coefficients[1] 
  b1[i]=lsfit$coefficients[2]
  b0[i];b1[i];summary(lsfit);
  Sxx=sum((x-mean(x))^2)
  Sxy=sum((x-mean(x))*(y-mean(y))) 
  e=lsfit$residuals
  SSe=sum(e^2)
  MSe[i]=SSe/(n-2) 
  varb1=MSe[i]/Sxx
  varb0=MSe[i]*(1/n+mean(x)^2/Sxx) 
  
  confint(lsfit) 
}
# make graphs
par(mfrow=c(1,3))
hist(b0, ylim=c(0,70),xlab="Beta 0",col=rgb(0,0,1,1/4), main="Histogram for beta 0",breaks = seq(-15,45,5))
hist(b1, ylim=c(0,50),xlab="Beta 1",col=rgb(0,0,1,1/4), main="Histogram for beta 1")
hist(MSe, xlab="Residual Mean Square",col=rgb(0,0,1,1/4), main="Histogram for residual mean square")
summary(lsfit)

# 4(d)
S=100 # the number of simulations
n=5
set.seed(2017)
b0=rep(NA,S)
b1=rep(NA,S)
MSe=rep(NA,S)
for (i in 1:S){
  x=c(2,6,10,14,18)
  error=rnorm(n,0,3)
  y=10+x*4+0.1*x^2+error
  
  lsfit=lm(y ~ x)
  b0[i]=lsfit$coefficients[1] 
  b1[i]=lsfit$coefficients[2]
  b0[i];b1[i];summary(lsfit);
  Sxx=sum((x-mean(x))^2)
  Sxy=sum((x-mean(x))*(y-mean(y))) 
  e=lsfit$residuals
  SSe=sum(e^2)
  MSe[i]=SSe/(n-2) 
  varb1=MSe[i]/Sxx
  varb0=MSe[i]*(1/n+mean(x)^2/Sxx) 
  
  confint(lsfit) 
}
# make graphs
par(mfrow=c(1,3))
hist(b0,ylim=c(0,30),xlab="Beta 0",col=rgb(0,0,1,1/4), main="Histogram for beta 0")
hist(b1,ylim=c(0,30),xlab="Beta 1",col=rgb(0,0,1,1/4), main="Histogram for beta 1")
hist(MSe, xlab="Residual Mean Square",col=rgb(0,0,1,1/4), main="Histogram for residual mean square")
summary(lsfit)

# 4(e)
S=100 # the number of simulations
n=5
set.seed(2017)
b0=rep(NA,S)
b1=rep(NA,S)
MSe=rep(NA,S)
for (i in 1:S){
  x=c(2,6,10,14,18)
  error=rnorm(n,0,3)
  y=10+x*4+0.2*x^2+error
  
  lsfit=lm(y ~ x)
  b0[i]=lsfit$coefficients[1] 
  b1[i]=lsfit$coefficients[2]
  b0[i];b1[i];summary(lsfit);
  Sxx=sum((x-mean(x))^2)
  Sxy=sum((x-mean(x))*(y-mean(y))) 
  e=lsfit$residuals
  SSe=sum(e^2)
  MSe[i]=SSe/(n-2) 
  varb1=MSe[i]/Sxx
  varb0=MSe[i]*(1/n+mean(x)^2/Sxx) 
  
  confint(lsfit) 
}
# make graphs
par(mfrow=c(1,3))
hist(b0,ylim=c(0,30),xlab="Beta 0",col=rgb(0,0,1,1/4), main="Histogram for beta 0")
hist(b1,ylim=c(0,30),xlab="Beta 1",col=rgb(0,0,1,1/4), main="Histogram for beta 1")
hist(MSe, xlab="Residual Mean Square",col=rgb(0,0,1,1/4), main="Histogram for residual mean square")
summary(lsfit)

# 4(e)(h)
x<-c(2,4,6,8,10,12,14,16,18) # x represents temperature
y<-c(5,11,10,13,22,23,30,28,32) # y represents heart rate
lsfit=lm(y ~ x)
summary(lsfit)
Sxx=sum((x-mean(x))^2)
Sxy=sum((x-mean(x))*(y-mean(y))) 
e=lsfit$residuals
SSe=sum(e^2)
MSe=SSe/(n-2) 
MSe

# 4(i)
x<-c(2,4,6,8,10,12,14,16,18) # x represents temperature
y<-c(5,11,10,13,22,23,30,28,32) # y represents heart rate
lsfit=lm(y ~ x)
summary(lsfit)
Sxx=sum((x-mean(x))^2)
Sxy=sum((x-mean(x))*(y-mean(y))) 
Syy=sum((y-mean(y))^2)
cc=Sxy/(Sxx^(1/2)*Syy^(1/2))
cc

# 4 95% confidence interval for E(Yh) when Xh = 7
# a
n=5
Xh = 7
xi=c(2,6,10,14,18)
yi=10+xi*4
xbar=c(10,10,10,10,10)
Yh=10+Xh*4 # E(Y|X)
muh.hat=Yh
sigmasquare.hat=1/(n-2)*sum((yi-yi.hat)^2)
t=qt(0.025,3)
var.hat=sigmasquare.hat*(1/n+(Xh-xbar)^2/sum((xi-xbar)^2))
CI.upper=muh.hat-t*var.hat
CI.lower=muh.hat+t*var.hat