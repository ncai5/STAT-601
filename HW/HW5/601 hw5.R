#1(a)
a=read.table("fungus.txt",header = TRUE)
Y=c(a$X1,a$X2,a$X3,a$X4,a$X5,a$X6,a$X7,a$X8)
temp=c("55", "60", "65", "70", "75", "80", "85","55", "60", "65", "70", "75", "80", "85",
       "55", "60", "65", "70", "75", "80", "85","55", "60", "65", "70", "75", "80", "85",
       "55", "60", "65", "70", "75", "80", "85","55", "60", "65", "70", "75", "80", "85",
       "55", "60", "65", "70", "75", "80", "85","55", "60", "65", "70", "75", "80", "85")
isolate=c("X1","X1","X1","X1","X1","X1","X1","X2","X2","X2","X2","X2","X2","X2","X3","X3","X3","X3","X3","X3","X3",
          "X4","X4","X4","X4","X4","X4","X4","X5","X5","X5","X5","X5","X5","X5","X6","X6","X6","X6","X6","X6","X6",
          "X7","X7","X7","X7","X7","X7","X7","X8","X8","X8","X8","X8","X8","X8")
lm.reg=lm(Y~isolate+temp)
summary(lm.reg)
anova(lm.reg)


#1(b)
fug = read.table("fungus.txt",header = T)
fug = unlist(fug)[-(1:7)]
fung = data.frame("fungus" = fug,"temp" = factor(rep(seq(55,85,by=5),8)),"iso" = factor(rep(1:8,each = 7)))
fung
temp

a=read.table("fungus.txt",header = TRUE)
a
Y=c(a$X1,a$X2,a$X3,a$X4,a$X5,a$X6,a$X7,a$X8)
x=rep(seq(55,85,by=5),8)
x2=x^2;x3=x^3;x4=x^4;x5=x^5;x6=x^6
lm6=lm(Y~x+x2+x3+x4+x5+x6)
anova(lm6)
lm0=lm(Y~1)
lm1=lm(Y~1+x)
lm2=lm(Y~1+x+x2)
lm3=lm(Y~x+x2+x3)
lm4=lm(Y~x+x2+x3+x4)
lm5=lm(Y~x+x2+x3+x4+x5)
anova(lm0,lm1)
anova(lm1,lm2)
anova(lm2,lm3)
anova(lm3,lm4)
anova(lm4,lm6)
anova(lm3,lm6)


#1(c)
lm3=lm(Y~x+x2+x3)
fun=function(x){
  return(-sum(c(1,x,x^2,x^3)*lm3$coefficients))
}
optimize(fun,c(55,85))

#2(a)
b=read.table("flushot.txt",header = TRUE)
b
glm.reg=glm(data=b,y~x1+x2+x3,family=binomial(link="logit"))
summary(glm.reg)
ci95=confint.default(glm.reg)

#2(b)
round(cbind(exp(glm.reg$coefficients),exp(confint.default(glm.reg))),3)

#2(c)
pnew = predict.glm(glm.reg, newdata=data.frame(x1=55,x2=60,x3=1), se.fit=T, type="link")
hat = pnew$fit
se.hat = pnew$se.fit
phat = exp(hat)/(1+exp(hat))
data.frame("lower bound"=hat-qnorm(0.975)*se.hat,"upper bound"=hat+qnorm(0.975)*se.hat) 
uphat = exp(hat-qnorm(0.975)*se.hat)/(1+exp(hat-qnorm(0.975)*se.hat))
lohat = exp(hat+qnorm(0.975)*se.hat)/(1+exp(hat+qnorm(0.975)*se.hat)) 
data.frame("lower bound"=uphat ,"upper bound"=lohat) # c.i for estimated prob

#2(e)
anova(glm(data=b,y~x1+x2,family = binomial("logit")),glm.reg,test = "Chisq")

#2(f)
b=read.table("flushot.txt",header = TRUE)
glm.reg=glm(data=b,y~x1+x2+x3,family=binomial(link="logit"))
glm0=glm(data=b,y~1,family = binomial("logit"))
#aic
step(glm0,scope=list(upper=glm.reg),direction = "both")
step(glm0,scope=list(upper=glm.reg),direction = "forward")
step(glm.reg)
#bic
step(glm0,scope=list(upper=glm.reg),direction = "both",k=log(length(y)))
step(glm0,scope=list(upper=glm.reg),direction = "forward",k=log(length(y)))
step(glm.reg,k=log(length(y)))
#final model
glm.reg=glm(data=b,y~x1+x2,family=binomial(link="logit"))
glm.reg
#2(g)
b=read.table("flushot.txt",header = TRUE)
glm.reg.final=glm(y~x1+x2,family=binomial(link="logit"))
install.packages("pROC")
library(pROC)
y=b$y
flushot.roc=roc(y~fitted(glm.reg.final))
plot(flushot.roc,cex.lab=1.5)
auc(flushot.roc)

#4(a)
par(mfrow=c(1,1))
kill.rate=c(3/50,5/49,19/47,19/38,24/49,35/50,2/50,14/49,20/50,27/50,41/50,40/50,28/50,37/50,46/50,48/50,48/50,50/50)
doses=c(2.00,2.64,3.48,4.59,6.06,8.00,2.00,2.64,3.48,4.59,6.06,8.00,2.00,2.64,3.48,4.59,6.06,8.00)
log(doses)
doses.killrate=lm(kill.rate~doses)
plot(log(doses),kill.rate,cex.lab=1.5)

#4(b)
data=data.frame("deposit"=c(2.00,2.64,3.48,4.59,6.06,8.00),
                "ddt"=c(3/50,5/49,19/47,19/38,24/49,35/50),
                "bhc"=c(2/50,14/49,20/50,27/50,41/50,40/50),
                "both"=c(28/50,37/50,46/50,48/50,48/50,50/50))
logit = function(x){
  log(x/(1-x))
}
data$deposit = log(data$deposit)
plot(data$deposit,logit(data$ddt),ylim=c(-4,4),xlab = "deposit",ylab = "logit(kill rate)",cex.lab=1.5)
points(data$deposit,logit(data$bhc),col="red")
points(data$deposit,logit(data$both),col="blue")

d = lm(logit(ddt)~deposit,data=data)
abline(reg=d)
b = lm(logit(bhc)~deposit,data=data)
abline(reg=b)
combine = lm(logit(data$both[1:5])~deposit[1:5],data=data)
abline(reg=combine)


#4(b)
par(mfrow=c(1,1))
kill.rate=c(3/50,5/49,19/47,19/38,24/49,35/50,2/50,14/49,20/50,27/50,41/50,40/50,28/50,37/50,46/50,48/50,48/50,50/50)
doses=c(2.00,2.64,3.48,4.59,6.06,8.00,2.00,2.64,3.48,4.59,6.06,8.00,2.00,2.64,3.48,4.59,6.06,8.00)
log(doses)
doses.killrate=lm(kill.rate~doses)

data=data.frame("doses"=c(2.00,2.64,3.48,4.59,6.06,8.00),
                "ddt"=c(3/50,5/49,19/47,19/38,24/49,35/50),
                "bhc"=c(2/50,14/49,20/50,27/50,41/50,40/50),
                "both"=c(28/50,37/50,46/50,48/50,48/50,50/50))
doses=c(2.00,2.64,3.48,4.59,6.06,8.00)
kill.rate.ddt=c(3/50,5/49,19/47,19/38,24/49,35/50)
kill.rate.bhc=c(2/50,14/49,20/50,27/50,41/50,40/50)
kill.rate.both=c(28/50,37/50,46/50,48/50,48/50,50/50)

#4(c)
insect=c(rep(0,6),rep(1,6),rep(2,6))
x1=c(rep(1,6),rep(0,6),rep(0,6))
x2=c(rep(0,6),rep(1,6),rep(0,6))

insc_type = factor(insect, levels = c(0, 1, 2),
                   labels = c("DDT", "BHC", "MIXED"))
dose=rep(c(2.00, 2.64, 3.48, 4.59, 6.06, 8.00),3)
logdose=log(dose)
killrate=c(3/50,5/49,19/47,19/38,24/49,35/50,2/50,14/49,20/50,27/50,41/50,40/50,28/50,37/50,46/50,48/50,48/50,50/50)
data=data.frame("killrate"=killrate,"deposit"=logdose,"x1"=x1,"x2"=x2)
logit = function(x){
  log(x/(1-x))
}
data.lm=lm((logit(killrate)[1:17]~data$deposit[1:17]+data$x1[1:17]+data$x2[1:17]))
data.aov1 = aov(logit(killrate)[1:17]~deposit[1:17]+x1[1:17]+x2[1:17]+x1[1:17]*deposit[1:17],data=data)
summary(data.aov1)
data.aov2 = aov(logit(killrate)[1:17]~deposit[1:17]+x1[1:17]+x2[1:17]+x1[1:17]*deposit[1:17],data=data)
summary(data.aov2)


#4(d)
insect=c(rep(0,6),rep(1,6),rep(2,6))
chem=factor(insect, levels = c(0, 1, 2),
            labels = c("DDT", "BHC", "MIXED"))
dose=rep(c(2.00, 2.64, 3.48, 4.59, 6.06, 8.00),3)
ldose=log(dose)
killrate=c(3/50,5/49,19/47,19/38,24/49,35/50,2/50,14/49,20/50,27/50,41/50,40/50,28/50,37/50,46/50,48/50,48/50,50/50)
lm.reg1=lm(killrate~chem+ldose)
lm.reg2=lm(killrate~chem+ldose-1)
#var-cov
summary(lm.reg1)$cov.unscaled
summary(lm.reg2)$cov.unscaled

confint(lm.reg2,level = 0.9)
#### 4 #####


insect=c(rep(0,6),rep(1,6),rep(2,6))

insc_type = factor(insect, levels = c(0, 1, 2),
                   labels = c("DDT", "BHC", "MIXED"))

dose=rep(c(2.00, 2.64, 3.48, 4.59, 6.06, 8.00),3)
killed=c(3,5,19,19,24,35,2,14,20,27,41,40,28,37,46,48,48,50)
unkilled=c(50,49,47,38,49,50,50,49,rep(50,10))-killed
data=data.frame("killed"=killed,"unkilled"=unkilled,"deposit"=dose,"type"=insc_type)

# making binary var 
y=c()
for(i in 1:length(killed)){
  yi=c(rep(1,killed[i]),rep(0,unkilled[i]))
  y=c(y,yi)
}

total=killed+unkilled
cumsum(total)
total=c(0,total)

XX = matrix(0,ncol = 2,nrow =sum(total))
X=as.matrix(data[,c(3,4)])
cum=cumsum(total)
for(i in 1:(length(cum)-1)){
  k=((cum[i]+1):(cum[i+1]))
  v=X[i,]
  for(j in 1:length(k)){
    XX[k[j],]=v
  }
}
XX=data.frame(XX)
XX$X1=as.numeric(as.character(XX$X1))
colnames(XX)=c("deposit","type")
head(XX)

glm(y~log(XX$deposit)+XX$type,family=binomial(link="logit"))
glm(y~log(XX$deposit)+XX$type-1,family=binomial(link="logit"))

ddt=c(rep(1,283),rep(0,299),rep(0,300))
bhc=c(rep(0,283),rep(1,299),rep(0,300))
chem=data.frame("ddt"=ddt,"bhc"=bhc)

glm(y~log(XX$deposit)+chem$ddt+chem$bhc,family=binomial(link="logit"))
glm(y~log(XX$deposit)+chem$ddt+chem$bhc-1,family=binomial(link="logit"))

  

#4(f)
insect=c(rep(0,6),rep(1,6),rep(2,6))

insc_type = factor(insect, levels = c(0, 1, 2),
                   labels = c("DDT", "BHC", "MIXED"))

dose=rep(c(2.00, 2.64, 3.48, 4.59, 6.06, 8.00),3)
killed=c(3,5,19,19,24,35,2,14,20,27,41,40,28,37,46,48,48,50)
unkilled=c(50,49,47,38,49,50,50,49,rep(50,10))-killed
data=data.frame("killed"=killed,"unkilled"=unkilled,"deposit"=dose,"type"=insc_type)

# making binary var 
y=c()
for(i in 1:length(killed)){
  yi=c(rep(1,killed[i]),rep(0,unkilled[i]))
  y=c(y,yi)
}

total=killed+unkilled
cumsum(total)
total=c(0,total)

XX = matrix(0,ncol = 2,nrow =sum(total))
X=as.matrix(data[,c(3,4)])
cum=cumsum(total)
for(i in 1:(length(cum)-1)){
  k=((cum[i]+1):(cum[i+1]))
  v=X[i,]
  for(j in 1:length(k)){
    XX[k[j],]=v
  }
}
XX=data.frame(XX)
XX$X1=as.numeric(as.character(XX$X1))
colnames(XX)=c("deposit","type")
head(XX)

glm1=glm(y~log(XX$deposit)+XX$type,family=binomial(link="logit"))
glm2=glm(y~log(XX$deposit)+XX$type-1,family=binomial(link="logit"))

summary(glm1)$cov.unscaled
summary(glm2)$cov.unscaled

confint(glm2,level = 0.9)

glm(y~log(XX$deposit)+XX$type,family=binomial(link="probit"))
glm(y~log(XX$deposit)+XX$type-1,family=binomial(link="probit"))

glm(y~log(XX$deposit)+XX$type,family=binomial(link="cloglog"))
glmc=glm(y~log(XX$deposit)+XX$type-1,family=binomial(link="cloglog"))
confint(glmc,level = 0.9)

glm(y~log(XX$deposit)+XX$type,family=binomial(link="loglog"))
glm(y~log(XX$deposit)+XX$type-1,family=binomial(link="loglog"))

glm.reg=glm(y~x1+x2+x3,family=binomial(link="probit"))
glm.reg=glm(y~x1+x2+x3,family=binomial(link="cloglog"))
glm.reg=glm(y~x1+x2+x3,family=binomial(link="loglog"))



#4(g)
dose=c(2.00, 2.64, 3.48, 4.59, 6.06, 8.00)
ldose=log(dose)
kill.rate.both=c(28/50,37/50,46/50,48/50,48/50,50/50)
lm.both=lm(kill.rate.both~ldose)
confint(lm.both,level = 0.9)
predict.lm(lm.both,newdata = data.frame(kill.rate.both=0.99),type ="response",interval="prediction")

