# final project
setwd("/Users/cainaiqing/Desktop/601/hw6")
#1
#a
calories=read.table("common_household_food.txt",header = T,sep=",")
calories=calories[1:961,]
###
caloriescov=calories[,2:22]
cov(caloriescov)
pairs(calories,main="Scatter Plot Matrix")
cor(calories$Weight,calories$Water)
cor(calories$Weight,calories$KCal)
"Scatter Plot Matrix"

#b
Fat=calories[,6]
SatFat=calories[,7]
MonoUnSatFat=calories[,8]
PolyUnSatFat=calories[,9]
protein=calories[,5]
carbohydrates=calories[,11]
cholesterol=calories[,10]
VitaA.IU.=calories[,17]
VitaA.RE.=calories[,18]
Thiamin=calories[,19]
Riboflavin=calories[,20]
Niacin=calories[,21]
VitaC=calories[,22]
Ca=calories[,12]
P=calories[,13]
Fe=calories[,14]
K=calories[,15]
Na=calories[,16]
weight=calories[,2]
water=calories[,3]
y=calories[,4]

lm=lm(y~Fat+protein+cholesterol+carbohydrates+Thiamin+K+weight)
summary(lm)
#
calories=read.table("common_household_food.txt",header = T,sep=",")
calories=calories[1:961,]
caloriess=calories[1:961,2:22]
boxplot(KCal~.,data=caloriess)
#transform y
library(car)
lambda=powerTransform(y,family = "bcnPower")
lambda

y=sqrt(y)


library(forecast)
x.bc=BoxCox(data4.1$V1,lambda=0.25)
qqnorm(x.bc)
qqline(x.bc)
y=sqrt(y)
#initial model
m0=lm(y~Fat+SatFat+MonoUnSatFat+PolyUnSatFat+
        protein+carbohydrates+cholesterol+VitaA.IU.+VitaA.RE.
      +Thiamin+Riboflavin+Niacin+VitaC+Ca+P+Fe+K+Na+weight+water)
summary(m0)
par(mfrow=c(1,1))
plot(m0)
#
install.packages("MASS")
library(MASS)
install.packages("lmtest")
install.packages("nortest")
library(nortest)
library(lmtest)
library(base)
par(mfrow=c(1,3))
plot(studres(m0),ylab="residuals",xlab = "index",main = "Scatterplot of Residuals",cex.lab=1.5,cex.main=1.5)
caloriesnew=calories[c(-422,-434,-420),]
Fat=caloriesnew[,6]
SatFat=caloriesnew[,7]
MonoUnSatFat=caloriesnew[,8]
PolyUnSatFat=caloriesnew[,9]
protein=caloriesnew[,5]
carbohydrates=caloriesnew[,11]
cholesterol=caloriesnew[,10]
VitaA.IU.=caloriesnew[,17]
VitaA.RE.=caloriesnew[,18]
Thiamin=caloriesnew[,19]
Riboflavin=caloriesnew[,20]
Niacin=caloriesnew[,21]
VitaC=caloriesnew[,22]
Ca=caloriesnew[,12]
P=caloriesnew[,13]
Fe=caloriesnew[,14]
K=caloriesnew[,15]
Na=caloriesnew[,16]
weight=caloriesnew[,2]
water=caloriesnew[,3]
y=caloriesnew[,4]

m0=lm(y~Fat+SatFat+MonoUnSatFat+PolyUnSatFat+
        protein+carbohydrates+cholesterol+VitaA.IU.+VitaA.RE.
      +Thiamin+Riboflavin+Niacin+VitaC+Ca+P+Fe+K+Na+weight+water)
summary(m0)
par(mfrow=c(2,2))
plot(m0)
#linear
## fit vs res ##
plot(m0$fitted.values ,studres(m0),xlab="Fitted Values",
     ylab="Studentized Residuals",
     main="Residual vs Fitted",cex.lab=1.5,cex.main=1.5) 
abline(h=0);abline(h=3,lty=2);abline(h=-3,lty=2)
## res QQ ##
qqnorm(studres(m0),ylab="Studentized Residuals",
       ylim=c(-2,2),cex.lab=1.5,cex.main=1.8)
qqline(studres(m0))

#vif
install.packages("car")
library(car)
vif=vif(m0)
mean(vif)
#bic both
n=961
step(m0,direction = "both",k=log(n))
#new model
m0new=lm(y~Fat+protein+carbohydrates+Thiamin++Ca+P+Fe+K+weight)
summary(m0new)
vifnew=vif(m0new)
vifnew
mean(vifnew)

# Multicollinearity
fats=calories[,6:9]
vitamins=calories[,17:22]
minerals=calories[,12:16]
pairs(fats)
pairs(vitamins)
pairs(minerals)
#
install.packages("car")
library(car)
m1=lm(y~Fat+SatFat+MonoUnSatFat+PolyUnSatFat+
        protein+carbohydrates+cholesterol+VitaA.IU.+VitaA.RE.
      +Thiamin+Riboflavin+Niacin+VitaC+Ca+P+Fe+K+Na+weight)
vif(m1)
par(mfrow=c(1,1))
plot(m1)
###

##data
par(mfrow=c(5,4))
plot(Fat);plot(SatFat);plot(MonoUnSatFat);plot(PolyUnSatFat);plot(protein);
plot(carbohydrates);plot(cholesterol);plot(VitaA.IU.);plot(VitaA.RE.);plot(Thiamin);
plot(Riboflavin);plot(Niacin);plot(VitaC);plot(Ca);plot(P);plot(Fe);plot(K);
plot(Na);plot(weight);plot(y)
##
m1=lm(y~Fat+SatFat+MonoUnSatFat+PolyUnSatFat+
        protein+carbohydrates+cholesterol+VitaA.IU.+VitaA.RE.
      +Thiamin+Riboflavin+Niacin+VitaC+Ca+P+Fe+K+Na+weight)

summary(m1)
plot(m1)
install.packages("MASS")
library(MASS)
install.packages("lmtest")
install.packages("nortest")
library(nortest)
library(lmtest)
library(base)

#scatterplot
par(mfrow=c(1,3))
plot(studres(m1),ylab="residuals",xlab = "index",main = "Scatterplot of Residuals",cex.lab=1.5,cex.main=1.5)
#linear
## fit vs res ##
plot(m1$fitted.values ,studres(m1),xlab="Fitted Values",
     ylab="Studentized Residuals",
     main="Residual vs Fitted",cex.lab=1.5,cex.main=1.5) 
abline(h=0);abline(h=3,lty=2);abline(h=-3,lty=2)
## res QQ ##
qqnorm(studres(m1),ylab="Studentized Residuals",
       ylim=c(-2,2),cex.lab=1.5,cex.main=1.8)
qqline(studres(m1))

##
par(mfrow=c(2,2))
lm.reg=lm(y~Fat+SatFat+MonoUnSatFat+PolyUnSatFat+
        protein+carbohydrates+cholesterol+VitaA.IU.+VitaA.RE.
      +Thiamin+Riboflavin+Niacin+VitaC+Ca+P+Fe+K+Na+weight)
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

#c
#aic backward
step(m1,direction = "backward",trace=1)

#aic
step(lm.reg,scope=list(upper=glm.reg),direction = "both")
step(glm0,scope=list(upper=glm.reg),direction = "forward")
step(glm.reg)
#bic
step(glm0,scope=list(upper=glm.reg),direction = "both",k=log(length(y)))
step(glm0,scope=list(upper=glm.reg),direction = "forward",k=log(length(y)))
step(glm.reg,k=log(length(y)))


#d
newdata=data.frame("Fat"=1.5,"SatFat"=0,"MonoUnSatFat"=0.5,"PolyUnSatFat"=0.5,
                     "protein"=3,"carbohydrates"=26,"cholesterol"=0,"VitaA.IU."=1250,"VitaA.RE."=125,
                  "Thiamin"=0,"Riboflavin"=0,"Niacin"=0,"VitaC"=30,
                   "Ca"=0,"P"=0,"Fe"=1.8,"K"=95,"Na"=85,"weight"=33)

predict.lm(m1,newdata,type="response",interval = "prediction")



#2
setwd("/Users/cainaiqing/Desktop/601/hw6")
audibility=read.csv("audibility.csv",header = T)
audibility
pairs(audibility)
row(audibility)

audibility$new_Carrier=factor(audibility$Carrier)
audibility$new_SPL=factor(audibility$SPL)
audibility$new_Ftest<-ifelse(audibility$Ftest <=0.05, 1, 0)


for (i in 1:nrow(audibility)) {
  if ((audibility$Carrier[i]=="a_F1") | (audibility$Carrier[i] =="u_F1") | ((audibility$Carrier[i]=="i_F1"))){
    audibility$Carriernew[i]="low"
  } else if((audibility$Carrier[i]=="a_F2") | (audibility$Carrier[i] =="u_F2") | ((audibility$Carrier[i]=="i_F2"))){
    audibility$Carriernew[i]="mid"
  } else{
    audibility$Carriernew[i]="high"
  }
}

audibility$Carrier[(audibility$Carrier=="a_F1") | (audibility$Carrier=="u_F1") | (audibility$Carrier=="i_F1"),]="low"


mnew=glm(outcome~Carriernew-1,data=audibility,family = binomial(link = "logit"))
summary(mnew)
mnewnew=glm(outcome~Carriernew-1,data=audibility,family = binomial(link = "logit"))
summary(mnewnew)


audibility$new_Carrier=factor(audibility$Carrier)
audibility$new_SPL=factor(audibility$SPL)
audibility$new_Ftest<-ifelse(audibility$Ftest <=0.05, 1, 0)
for (i in 1:nrow(audibility)) {
  if (((audibility$SL[i]>0)&(audibility$Ftest[i] < 0.05)) | ((audibility$SL[i]<0)&(audibility$Ftest[i] >=0.05))){
    audibility$outcomeF[i]=1
  } else{
    audibility$outcomeF[i]=0
  }
}
for (i in 1:nrow(audibility)) {
  if (((audibility$SL[i]>0)&(audibility$Rayleigh[i] < 0.05)) | ((audibility$SL[i]<0)&(audibility$Rayleigh[i] >=0.05))){
    audibility$outcomeR[i]=1
  } else{
    audibility$outcomeR[i]=0
  }
}
m121=glm(outcomeF~new_Carrier,data=audibility,family = binomial(link = "logit"))
summary(m121)
anova(m121)
plot(m121$residuals,main = "Scatterplot of Residuals",cex.lab=1.5,cex.main=1.5)
plot(m121)
m122=glm(outcomeR~new_Carrier,data=audibility,family = binomial(link = "logit"))
summary(m122)
anova(m122)
plot(m122)



