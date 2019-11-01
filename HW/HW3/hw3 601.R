#2 linear 
x=c(9,9,9,7,7,7,5,5,5,3,3,3,1,1,1)
y=c(0.07,0.09,0.08,0.16,0.17,0.21,0.49,0.58,0.53,1.22,1.15,1.07,2.84,2.57,3.10)
plot(x,y,xlab="time",ylab="concentration",main = "concentration v.s time",cex.lab=1.5, cex.main=2)
lsfit=lm(y ~ x)
abline(lsfit)
#2 equal variance
x=c(9,9,9,7,7,7,5,5,5,3,3,3,1,1,1)
y=c(0.07,0.09,0.08,0.16,0.17,0.21,0.49,0.58,0.53,1.22,1.15,1.07,2.84,2.57,3.10)
lsfit=lm(y ~ x)
lsfit
confint(lsfit) 
e=lsfit$residuals
fitted.values=lsfit$coefficients[1]+lsfit$coefficients[2]*x
plot(fitted.values,e,xlab = "Fitted values",ylab = "Residuals",cex.lab=1.6)
abline(h=0,lty="dotted")

#2 qq-plot
x=c(9,9,9,7,7,7,5,5,5,3,3,3,1,1,1)
y=c(0.07,0.09,0.08,0.16,0.17,0.21,0.49,0.58,0.53,1.22,1.15,1.07,2.84,2.57,3.10)
lsfit=lm(y ~ x)
lsfit
confint(lsfit) 
e=lsfit$residuals
qqnorm(e,main = "Q-Q Plot of residuals",xlab = "Theoretical of Quantiles",ylab = "Sample Quantiles",cex.lab=1.5,cex.main=2)
qqline(e, col=2, lwd=2)

# 2(f)
x=c(9,9,9,7,7,7,5,5,5,3,3,3,1,1,1)
y=c(0.07,0.09,0.08,0.16,0.17,0.21,0.49,0.58,0.53,1.22,1.15,1.07,2.84,2.57,3.10)
y_log=log(y)
lsfit=lm(y_log ~ x)
lsfit
summary(lsfit)
