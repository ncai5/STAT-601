# 4. 95% confidence interval for E(Yh) when Xh = 7
x=c(2,6,10,14,18)
summary(lm(10+4*x+rnorm(5,0,3)~x))
par(mfrow=c(3,1))
beta0=replicate(n=100,coefficients(lm(10+4*x+rnorm(5,0,3)~x))[1])
hist(beta0,main=bquote("Histogram of"*hat(beta[0])*"(Problem (a))"))
beta1=replicate(n=100,coefficients(lm(10+4*x+rnorm(5,0,3)~x))[2])
hist(beta1,main=bquote("Histogram of"*hat(beta[1])*"(Problem (a))"))
sigmahat2=replicate(n=100,(summary(lm(10+4*x+rnorm(5,0,3)~x))$sigma)^2)
hist(sigmahat2,main=bquote("Histogram of "*hat(sigma)^2*"(Problem (a))"))
# a
x=c(2,6,10,14,18)
predict100=replicate(100,predict(lm(10+4*x+rnorm(5,0,3)~x),newdata=data.frame(x=7),interval = "confidence")[-1])
xh=7
EY=10+4*xh
n=0
for (i in 1:100){
  if (EY>predict100[1,i]&EY<predict100[2,i]){
    n=n+1
  }
}
n
probability=n/100
probability

# b
x=c(2, 4, 6, 8, 10, 12, 14, 16, 18, 20)
predict100=replicate(100,predict(lm(10+4*x+rnorm(10,0,3)~x),newdata=data.frame(x=7),interval = "confidence")[-1])
xh=7
EY=10+4*xh
n=0
for (i in 1:100){
  if (EY>predict100[1,i]&EY<predict100[2,i]){
    n=n+1
  }
}
n
probability=n/100
probability

# c
x=c(2, 6, 10, 14, 18, 22, 26, 30, 34, 38)
predict100=replicate(100,predict(lm(10+4*x+rnorm(10,0,3)~x),newdata=data.frame(x=7),interval = "confidence")[-1])
xh=7
EY=10+4*xh
n=0
for (i in 1:100){
  if (EY>predict100[1,i]&EY<predict100[2,i]){
    n=n+1
  }
}
n
probability=n/100
probability

# d
x=c(2, 6, 10, 14, 18)
predict100=replicate(100,predict(lm(10+4*x+0.1*x^2+rnorm(5,0,3)~x),newdata=data.frame(x=7),interval = "confidence")[-1])
xh=7
EY=10+4*xh+0.1*xh^2
n=0
for (i in 1:100){
  if (EY>predict100[1,i]&EY<predict100[2,i]){
    n=n+1
  }
}
n
probability=n/100
probability

# e
x=c(2, 6, 10, 14, 18)
predict100=replicate(100,predict(lm(10+4*x+0.2*x^2+rnorm(5,0,3)~x),newdata=data.frame(x=7),interval = "confidence")[-1])
xh=7
EY=10+4*xh+0.2*xh^2
n=0
for (i in 1:100){
  if (EY>predict100[1,i]&EY<predict100[2,i]){
    n=n+1
  }
}
n
probability=n/100
probability