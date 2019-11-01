

# coefficient estimates
x<-c(2, 4, 6, 8, 10, 12, 14, 16, 18)
y<-c(5, 11, 10, 13, 22, 23, 30, 28, 32)
x.bar = mean(x); x.bar
y.bar = mean(y); y.bar
sum((x-x.bar)*(y - y.bar))
sum((x - x.bar)^2)
b1 = sum((x - x.bar)*(y - y.bar))/sum((x - x.bar)^2)
b0 = y.bar - b1*x.bar
b0; b1
summary(lm.reg)

### drawing power curve 


# generating mock data
x=seq(3,30,by=3)
error=rnorm(10,0,4)
y=20+x*2+error
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

# calculating power

power=function(b,s,a,Sxx){
  z=qnorm(1-a/2)
  print(z)
  print(z-b/s*sqrt(Sxx))
  print(-z-b/s*sqrt(Sxx))
  print(pnorm(z-b/s*sqrt(Sxx))) 
  print(pnorm(-z-b/s*sqrt(Sxx))) 
  print(1-pnorm(z-b/s*sqrt(Sxx))+pnorm(-z-b/s*sqrt(Sxx)))
}
power(1.5,10,0.05,Sxx) #power when beta1 is 1.5, sigma is 10 and alpha =0.05

#make a function for calculation power
power.plot=function(b,s,a,Sxx){
  z=qnorm(1-a/2) 
  (1-pnorm(z-b/s*sqrt(Sxx))+pnorm(-z-b/s*sqrt(Sxx)))
}
power.plot(1.5,10,0.05,Sxx)

b.seq=seq(-1.5,1.5,0.01)
b.power=power.plot(b.seq,10,0.05,Sxx)
plot(b.seq,b.power,type="l",xlab=expression(beta[1]),ylab="power"
,main="power curve(sigma=10)",xlim=c(-1.5,1.5),ylim=c(0,1))
segments(-1.5,0.05,1.5,0.05,col="blue",lty=2)
text(-1.3,0.1,"0.05",col="blue")

