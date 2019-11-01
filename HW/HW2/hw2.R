# 2(a)
x<-c(2,4,6,8,10,12,14,16,18) # x represents temperature
y<-c(5,11,10,13,22,23,30,28,32) # y represents heart rate
p <- plot(x,y, xlab="Temperature" ,ylab="Heart Rate", pch=16,col="blue",main="Scatter Plot of Heart Rate vs. Temperature")
abline(lm(y ~ x), col="red", lwd=2, lty=1)       
# 4(a)
x=c(2,6,10,14,18)
error=rnorm(5,0,9)
y=10+x*4+error
n=5

lsfit=lm(y ~ x)
b0=lsfit$coefficients[1] 
b1=lsfit$coefficients[2]
b0;b1;summary(lsfit);
Sxx=sum((x-mean(x))^2)
Sxy=sum((x-mean(x))*(y-mean(y))) 
e=lsfit$residuals
SSe=sum(e^2)
MSe=SSe/(n-2) 
varb1=MSe/Sxx
varb0=MSe*(1/n+mean(x)^2/Sxx) 

confint(lsfit) 

# 4(a) 100times
S=100 # the number of simulations
n=5
set.seed(2017)
b0=rep(NA,S)
b1=rep(NA,S)
MSe=rep(NA,S)
for (i in 1:S){
  x=c(2,6,10,14,18)
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

par(mfrow=c(1,3))
hist(b0, ylim=c(0,40),xlab="Beta 0",col=rgb(0,0,1,1/4), main="Histogram for beta 0")
hist(b1, xlim=c(3,5), ylim=c(0,20),xlab="Beta 1",
     col=rgb(0,0,1,1/4), main="Histogram for beta 1")
hist(MSe, xlab="Residual Mean Square",col=rgb(0,0,1,1/4), main="Histogram for residual mean square")

y.hat=predict(lsfit,newdata = 7)
y.hat
summary(lsfit)


# 5(b)
# calculating power
x<-c(2, 4, 6, 8, 10, 12, 14, 16, 18)
y<-c(5, 11, 10, 13, 22, 23, 30, 28, 32)
Sxx=sum((x-mean(x))^2)
Sxy=sum((x-mean(x))*(y-mean(y))) 
power=function(b,s,a,Sxx){
  z=qnorm(1-a/2)
  print(z)
  print(z-b/s*sqrt(Sxx))
  print(-z-b/s*sqrt(Sxx))
  print(pnorm(z-b/s*sqrt(Sxx))) 
  print(pnorm(-z-b/s*sqrt(Sxx))) 
  print(1-pnorm(z-b/s*sqrt(Sxx))+pnorm(-z-b/s*sqrt(Sxx)))
}
power(0,2.5,0.05,Sxx) #power when beta1 is 0, sigma is 2.5 and alpha =0.05
power(0.5,2.5,0.05,Sxx) #power when beta1 is 0, sigma is 2.5 and alpha =0.05
power(-0.5,2.5,0.05,Sxx) #power when beta1 is 0, sigma is 2.5 and alpha =0.05
power(1,2.5,0.05,Sxx) #power when beta1 is 1, sigma is 2.5 and alpha =0.05
power(-1,2.5,0.05,Sxx) #power when beta1 is -1, sigma is 2.5 and alpha =0.05
power(1.5,2.5,0.05,Sxx) #power when beta1 is 1.5, sigma is 2.5 and alpha =0.05
power(-1.5,2.5,0.05,Sxx) #power when beta1 is -1.5, sigma is 2.5 and alpha =0.05

# 5(c)
x<-c(2, 4, 6, 8, 10, 12, 14, 16, 18)
y<-c(5, 11, 10, 13, 22, 23, 30, 28, 32)
Sxx=sum((x-mean(x))^2)
power.plot=function(b,s,a,Sxx){
  z=qnorm(1-a/2) 
  (1-pnorm(z-b/s*sqrt(Sxx))+pnorm(-z-b/s*sqrt(Sxx)))
}

b.seq=seq(-1.5,1.5,0.01)
b.power=power.plot(b.seq,2.5,0.05,Sxx)
b.power
plot(b.seq,b.power,type="l",xlab=expression(beta[1]),ylab="power"
     ,main="power curve(sigma=2.5)",xlim=c(-1.5,1.5),ylim=c(0,1))
segments(-1.5,0.05,1.5,0.05,col="blue",lty=2)
text(-1.3,0.1,"0.05",col="blue")
# 5(c)
x<-c(4, 8, 12, 16)
y<-c(11, 13, 23, 28)
Sxx=sum((x-mean(x))^2)
power.plot=function(b,s,a,Sxx){
  z=qnorm(1-a/2) 
  (1-pnorm(z-b/s*sqrt(Sxx))+pnorm(-z-b/s*sqrt(Sxx)))
}

b.seq=seq(-1.5,1.5,0.01)
b.power=power.plot(b.seq,2.5,0.05,Sxx)
b.power
plot(b.seq,b.power,type="l",xlab=expression(beta[1]),ylab="power"
     ,main="power curve(sigma=2.5)",xlim=c(-1.5,1.5),ylim=c(0,1))
segments(-1.5,0.05,1.5,0.05,col="blue",lty=2)
text(-1.3,0.1,"0.05",col="blue")



#
x<-c(2,4,6,8,10,12,14,16,18) # x represents temperature
y<-c(5,11,10,13,22,23,30,28,32) # y represents heart rate
error=rnorm(10,0,4)
n=10

lsfit=lm(y ~ x)
b0=lsfit$coefficients[1] 
b1=lsfit$coefficients[2]
b0;b1;summary(lsfit);
Sxx=sum((x-mean(x))^2)
Sxy=sum((x-mean(x))*(y-mean(y))) 
e=lsfit$residuals
SSe=sum(e^2)
MSe=SSe/(n-2) 
varb1=MSe/Sxx
varb0=MSe*(1/n+mean(x)^2/Sxx) 

confint(lsfit) 

# 5(a)
x<-c(2, 4, 6, 8, 10, 12, 14, 16, 18)
y<-c(5, 11, 10, 13, 22, 23, 30, 28, 32)
x.bar = mean(x); x.bar
y.bar = mean(y); y.bar
sum((x-x.bar)*(y - y.bar))
sum((x - x.bar)^2)
b1 = sum((x - x.bar)*(y - y.bar))/sum((x - x.bar)^2)
b0 = y.bar - b1*x.bar
b0; b1
lsfit=lm(y ~ x)
summary(lsfit);
e=lsfit$residuals
SSe=sum(e^2)
MSe=SSe/(n-2) 
confint(lsfit)
