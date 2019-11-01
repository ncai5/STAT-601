#hw6
#1
SES=c("A","A","A","A","B","B","B","B","C","C","C","C","D","D","D","D","E","E","E","E",
      "F","F","F","F")
mental=c("Well","Mild","Moderate","Impaired","Well","Mild","Moderate","Impaired",
         "Well","Mild","Moderate","Impaired","Well","Mild","Moderate","Impaired",
         "Well","Mild","Moderate","Impaired","Well","Mild","Moderate","Impaired")
total=c(64,94,58,46,57,94,54,40,57,105,65,60,72,141,77,94,36,97,54,78,21,71,54,71)
logn=log(total)
y=c(64,94,58,46,57,94,54,40,57,105,65,60,72,141,77,94,36,97,54,78,21,71,54,71)
#regression
glm1=glm(y~mental,family = poisson(link="log"))
summary(glm1)
glm2=glm(y~SES,family = poisson(link="log"))
summary(glm2)
glm11=glm(y~SES+mental,family = poisson(link="log"))
summary(glm11)
anova(glm11)
glm12=glm(y~SES+mental+SES*mental,family = poisson(link="log"))
anova(glm12)
summary(glm12)
glm13=glm(y~SES+SES*mental,family = poisson(link="log"))
anova(glm13)
glm14=glm(y~mental+SES*mental,family = poisson(link="log"))
anova(glm14)

#######
Mentalhealthstatus.well=c(64,57,57,72,36,21)
Mentalhealthstatus.mild=c(94,94,105,141,97,71)
Mentalhealthstatus.moderate=c(58,54,65,77,54,54)
Mentalhealthstatus.Impaired=c(46,40,60,94,78,71)
Mentalhealth=data.frame("well"=Mentalhealthstatus.well,"mild"=Mentalhealthstatus.mild,
                        "moderate"=Mentalhealthstatus.moderate,"impaired"=Mentalhealthstatus.Impaired)

reg.lm=lm(SES~well+mild+moderate+impaired,data=Mentalhealth)
summary(reg.lm)
summary.aov(reg.lm)

#2
#a
install.packages("BradleyTerry2")
library(BradleyTerry2)
data("baseball",package = "BradleyTerry2")
head(baseball)
#original model
baseballModel1=BTm(cbind(home.wins,away.wins),home.team, away.team,data = baseball,id="team")
baseballModel1
summary(baseballModel1)
#extended model
baseball$home.team=data.frame(team=baseball$home.team,at.home=1)
baseball$away.team=data.frame(team=baseball$away.team,at.home=0)
baseballModel2=update(baseballModel1,formula= ~ team+at.home)
summary(baseballModel2)
#Obtain the likelihood-ratio statistic
anova(baseballModel1,baseballModel2)
1-pchisq(5.4106,1)

#d
BTabilities(baseballModel1)
par(mfrow=c(2,2))
plot(baseballModel2,cex.lab=1.5,cex.main=1.5)
residual=residuals(baseballModel2)
par(mfrow=c(1,1))
plot(residual,main = "Scatterplot",cex.lab=1.5,cex.main=1.5)

BTresiduals(baseballModel1)
BTresiduals(baseballModel2)


#3
#a&b
fiji=read.table("fiji.txt",header = T)

marriage1=c(rep(0,4),rep(1,4),rep(0,15),rep(0,4),rep(1,4),rep(0,15))
marriage2=c(rep(0,8),rep(1,4),rep(0,11),rep(0,8),rep(1,4),rep(0,11))
marriage3=c(rep(0,12),rep(1,4),rep(0,7),rep(0,12),rep(1,4),rep(0,7))
marriage4=c(rep(0,16),rep(1,4),rep(0,3),rep(0,16),rep(1,4),rep(0,3))
marriage5=c(rep(0,20),rep(1,3),rep(0,20),rep(1,3))
marriage=data.frame("5-9"=marriage1,"10-14"=marriage2,"15-19"=marriage3,"20-24"=marriage4,"25+"=marriage5)

edu1=c(0,1,0,0,0,1,0,0,0,1,0,0,0,1,0,0,0,1,0,0,0,1,0,0,1,0,0,0,1,0,0,0,1,0,0,0,1,0,0,0,1,0,0,0,1,0)
edu2=c(0,0,1,0,0,0,1,0,0,0,1,0,0,0,1,0,0,0,1,0,0,0,1,0,0,1,0,0,0,1,0,0,0,1,0,0,0,1,0,0,0,1,0,0,0,1)
edu3=c(0,0,0,1,0,0,0,1,0,0,0,1,0,0,0,1,0,0,0,1,0,0,0,0,0,0,1,0,0,0,1,0,0,0,1,0,0,0,1,0,0,0,1,0,0,0)
edu=data.frame("lower elementary"=edu1,"upper elementary"=edu2,"secondary or higher"=edu3)
place1=c(rep(0,23),rep(1,23))

place=data.frame("rural"=place1)
XX=data.frame("5-9"=marriage1,"10-14"=marriage2,"15-19"=marriage3,"20-24"=marriage4,"25+"=marriage5,"lower elementary"=edu1,"upper elementary"=edu2,"secondary or higher"=edu3,"rural"=place1)
average=fiji$average
averagenew=c()
averagenew=c(average[1:23],average[25:47])
tot=fiji$tot
totnew=c()
totnew=c(tot[1:23],tot[25:47])
y=averagenew*totnew
log.n=log(totnew)
#read data
fiji=read.table("fiji.txt",header = T)
fijinew=fiji[c(-24,-48),]
fijinew$marriage=factor(fijinew$marriage)
fijinew$edu=factor(fijinew$edu)
fijinew$abode=factor(fijinew$abode)
y=fijinew$tot*fijinew$average
log.n=log(fijinew$tot)
#model
glm1=glm(data=XX,round(y)~marriage1+marriage2+marriage3+marriage4+
           marriage5+edu1+edu2+edu3+place1,offset=log.n,family = poisson(link="log"))

glmnew=glm(data=fijinew,round(y)~marriage+edu+abode,offset=log.n,family = poisson(link="log"))
confint(glmnew)
glmnewnew=glm(data=fijinew,round(y)~marriage+edu+abode+marriage*edu+marriage*abode+edu*abode+
             marriage*edu*abode,offset=log.n,family = poisson(link="log"))
summary(glmnew)
anova(glmnew)
glm2=glm(data=XX,round(y)~marriage1+marriage2+marriage3+marriage4+
           marriage5+edu1+edu2+edu3+place1,offset=log.n,family = poisson(link="log"))


marriagee=factor(fiji$marriage,levels = c(1,2,3,4,5,6),labels = c("<5","5-9","10-14","15-19","20-24","25+"))
eduu=factor(fiji$edu,levels = c(1,2,3,4),labels=c("none","lower elementary","upper elementary","secondary or higher"))
placee=factor(fiji$abode,levels = c(1,2),labels = c("urban","rural"))

#checking
install.packages("MASS")
library(MASS)
install.packages("lmtest")
install.packages("nortest")
library(nortest)
library(lmtest)
library(base)

par(mfrow=c(2,2))
plot(glm1)
#scatterplot
par(mfrow=c(1,1))
plot(studres(glm1),ylab="residuals",xlab = "index",main = "scatterplot of residuals",cex.lab=1.5,cex.main=1.5)
## fit vs res ##
plot(glm1$fitted.values ,studres(glm1),xlab="Fitted values",
     ylab="Studentized residuals",
     main="Residual vs Fitted",cex.lab=1.5,cex.main=1.5) 
abline(h=0);abline(h=3,lty=2);abline(h=-3,lty=2)
## res QQ ##
qqnorm(studres(glm1),ylab="Studentized residuals",
       ylim=c(-2,2),cex.lab=1.5,cex.main=1.8)
qqline(studres(glm1))
## Shapiro -Wilk test
shapiro.test(studres(glm1))

#b
summary(glm1)
#c
#predict
pnew1=predict.glm(glm1,newdata = data.frame("marriage1"=0,"marriage2"=1,"marriage3"=0,"marriage4"=0,
                                           "marriage5"=0,"edu1"=0,"edu2"=1,"edu3"=0,"place1"=0,"log.n"=0), type="link",se.fit = T)
pnew1
pnew2=predict.glm(glm1,newdata = data.frame("marriage1"=0,"marriage2"=0,"marriage3"=1,"marriage4"=0,
                                           "marriage5"=0,"edu1"=0,"edu2"=1,"edu3"=0,"place1"=0,"log.n"=0), type="link",se.fit = T)
pnew2
pnew3=predict.glm(glm1,newdata = data.frame("marriage1"=0,"marriage2"=0,"marriage3"=0,"marriage4"=1,
                                           "marriage5"=0,"edu1"=0,"edu2"=1,"edu3"=0,"place1"=0,"log.n"=0), type="link",se.fit = T)
pnew3
pnew4=predict.glm(glm1,newdata = data.frame("marriage1"=0,"marriage2"=0,"marriage3"=0,"marriage4"=0,
                                           "marriage5"=1,"edu1"=0,"edu2"=1,"edu3"=0,"place1"=0,"log.n"=0), type="link",se.fit = T)
pnew4

pnew10=predict.glm(glm1,newdata = data.frame("marriage1"=1,"marriage2"=0,"marriage3"=0,"marriage4"=0,
                                           "marriage5"=0,"edu1"=0,"edu2"=1,"edu3"=0,"place1"=0,"log.n"=0), type="response",se.fit = T)
pnew25=predict.glm(glm1,newdata = data.frame("marriage1"=0,"marriage2"=0,"marriage3"=0,"marriage4"=0,"marriage5"=1,"edu1"=0,"edu2"=0,"edu3"=1,"place1"=1,"log.n"=0), type="link",se.fit = T)
pnew25
#ci
yhat=pnew$fit

se.yhat=pnew$se.fit
c((yhat-qnorm(0.975)*se.yhat),yhat+qnorm(0.975)*se.yhat)
c(exp(yhat-qnorm(0.975)*se.yhat),exp(yhat+qnorm(0.975)*se.yhat))

#d
pnew2=predict.glm(glm1,newdata = data.frame("marriage1"=0,"marriage2"=0,"marriage3"=0,"marriage4"=0,"marriage5"=1,"edu1"=0,"edu2"=0,"edu3"=1,"place1"=1,"log.n"=0), 
                          type="response",se.fit = T)
#ci
yhat2=pnew2$fit
se.yhat2=pnew2$se.fit
c((yhat2-qnorm(0.975)*se.yhat2),yhat2+qnorm(0.975)*se.yhat2)

c(exp(yhat2-qnorm(0.95)*se.yhat2),exp(yhat2+qnorm(0.95)*se.yhat2))


#4
byss=read.table("byss.txt",header = T)
byss
length(byss$affected)
byss$race=factor(byss$race)
byss$sex=factor(byss$sex)
byss$smok=factor(byss$smok)
byss$empl=factor(byss$empl)
byss$dust=factor(byss$dust)
m1=glm(cbind(affected,not_affected)~race+sex+smok+empl+dust,data=byss,family = binomial(link="logit"))
summary(m1)
confint(m1,level = 0.9)
m2=glm(cbind(affected,not_affected)~race+smok+empl+dust,data=byss,family = binomial(link="logit"))
summary(m2)
m3=glm(cbind(affected,not_affected)~smok+empl+dust,data=byss,family = binomial(link="logit"))
summary(m3)
############
y=c()
for (i in 1:length(byss$affected)){
  yi=c(rep(1,byss$affected[i]),rep(0,byss$not_affected[i]))
  y=c(y,yi)
}
y
length(y)

race=c()
for (j in 1:length(byss$race)) {
  if (byss$race[j]==2){
    racej=c(rep(1,byss$affected[j]+byss$not_affected[j]))
    race=c(race,racej)
  } else{
    racej=c(rep(0,byss$affected[j]+byss$not_affected[j]))
    race=c(race,racej)
  }
}

sex=c()
for (k in 1:length(byss$sex)) {
  if (byss$sex[k]==2){
    sexk=c(rep(1,byss$affected[k]+byss$not_affected[k]))
    sex=c(sex,sexk)
  } else{
    sexk=c(rep(0,byss$affected[k]+byss$not_affected[k]))
    sex=c(sex,sexk)
  }
}

smokinghabit=c()
for (l in 1:length(byss$smok)) {
  if (byss$smok[l]==1){
    smokinghabitl=c(rep(0,byss$affected[l]+byss$not_affected[l]))
    smokinghabit=c(smokinghabit,smokinghabitl)
  } else{
    smokinghabitl=c(rep(1,byss$affected[l]+byss$not_affected[l]))
    smokinghabit=c(smokinghabit,smokinghabitl)
  }
}

employment1=c()
for ( m in 1:length(byss$empl)) {
  if (byss$empl[m]==2){
    employment1m=c(rep(1,byss$affected[m]+byss$not_affected[m]))
    employment1=c(employment1,employment1m)
  } else{
    employment1m=c(rep(0,byss$affected[m]+byss$not_affected[m]))
    employment1=c(employment1,employment1m)
  }
}
employment2new=employment1

employment2=c()
for ( n in 1:length(byss$empl)) {
  if (byss$empl[n]==3){
    employment2n=c(rep(1,byss$affected[n]+byss$not_affected[n]))
    employment2=c(employment2,employment2n)
  } else{
    employment2n=c(rep(0,byss$affected[n]+byss$not_affected[n]))
    employment2=c(employment2,employment2n)
  }
}
employment3new=employment2

dust1=c()
for ( o in 1:length(byss$dust)) {
  if (byss$dust[o]==2){
    dust1o=c(rep(1,byss$affected[o]+byss$not_affected[o]))
    dust1=c(dust1,dust1o)
  } else{
    dust1o=c(rep(0,byss$affected[o]+byss$not_affected[o]))
    dust1=c(dust1,dust1o)
  }
}
dust2new=dust1
dust2=c()
for ( p in 1:length(byss$dust)) {
  if (byss$dust[p]==3){
    dust2p=c(rep(1,byss$affected[p]+byss$not_affected[p]))
    dust2=c(dust2,dust2p)
  } else{
    dust2p=c(rep(0,byss$affected[p]+byss$not_affected[p]))
    dust2=c(dust2,dust2p)
  }
}
dust3new=dust2

glm2=glm(y~race+sex+smokinghabit+employment2new+employment3new+dust2new+dust3new,family = binomial(link="logit"))
summary(glm2)
#d
glmfinal1=glm(y~sex+smokinghabit+employment2new+employment3new+dust3new+employment2new*dust3new+sex*dust3new,
             family = binomial(link="logit"))
glmfinal1
summary(glmfinal1)

glmfinal2=glm(y~sex+smokinghabit+employment2new+employment3new+dust3new+sex*dust3new,
             family = binomial(link="logit"))
glmfinal2
summary(glmfinal2)
anova(glmfinal2)
#b
confint(m1,level=0.9)

#c
m2=glm(cbind(affected,not_affected)~race+sex+smok+empl+dust,data=byss,family = binomial(link="logit"))
summary(m2)

glm3=glm(y~race+sex+smokinghabit+employment2new+employment3new+dust3new,family = binomial(link="logit"))
summary(glm3)
glm4=glm(y~sex+smokinghabit+employment2new+employment3new+dust3new,family = binomial(link="logit"))
summary(glm4)
glm5=glm(y~smokinghabit+employment2new+employment3new+dust3new,family = binomial(link="logit"))
summary(glm5)

#d
glminter1=glm(cbind(affected,not_affected)~race+sex+smok+empl+dust+race*sex,data=byss,family = binomial(link="logit"))
glminter1$coefficients
summary(glminter1)
glminter2=glm(cbind(affected,not_affected)~race+sex+smok+empl+dust+race*smok,data=byss,family = binomial(link="logit"))
summary(glminter2)
glminter3=glm(cbind(affected,not_affected)~race+sex+smok+empl+dust+race*empl,data=byss,family = binomial(link="logit"))
summary(glminter3)
glminter4=glm(cbind(affected,not_affected)~race+sex+smok+empl+dust+race*dust,data=byss,family = binomial(link="logit"))
summary(glminter4)
glminter5=glm(cbind(affected,not_affected)~race+sex+smok+empl+dust+sex*smok,data=byss,family = binomial(link="logit"))
summary(glminter5)
glminter6=glm(cbind(affected,not_affected)~race+sex+smok+empl+dust+sex*empl,data=byss,family = binomial(link="logit"))
summary(glminter6)
glminter7=glm(cbind(affected,not_affected)~race+sex+smok+empl+dust+sex*dust,data=byss,family = binomial(link="logit"))
summary(glminter7)
glminter8=glm(cbind(affected,not_affected)~race+sex+smok+empl+dust+smok*empl,data=byss,family = binomial(link="logit"))
summary(glminter8)
glminter9=glm(cbind(affected,not_affected)~race+sex+smok+empl+dust+smok*dust,data=byss,family = binomial(link="logit"))
summary(glminter9)
glminter10=glm(cbind(affected,not_affected)~race+sex+smok+empl+dust+empl*dust,data=byss,family = binomial(link="logit"))
summary(glminter10)

glmfinal=glm(cbind(affected,not_affected)~sex+smok+empl+dust+sex*dust,data=byss,family = binomial(link="logit"))
summary(glmfinal)
anova(glmfinal)

#4(b)
femaleaffected=0
femalenotaffected=0
maleaffected=0
malenotaffected=0
for (k in 1:length(byss$sex)) {
  if (byss$sex[k]==1){
    femaleaffected=femaleaffected+byss$affected[k]
    femalenotaffected=femalenotaffected+byss$not_affected[k]
  } else{
    maleaffected=maleaffected+byss$affected[k]
    malenotaffected=malenotaffected+byss$not_affected[k]
  }
}

femaleodds=femaleaffected/(femaleaffected+femalenotaffected)
maleodds=maleaffected/(maleaffected+malenotaffected)

#4(e)
#dust and risk
plot(dust1,y)
plot(dust2,y)
glmdust=glm(y~dust2new+dust3new,family = binomial(link="logit"))
par(mfrow=c(1,1))
abline(glmdust)
curve(expr = exp(glmdust$coefficients[1]+glmdust$coefficients[2]*x)/(1+exp(glmdust$coefficients[1]+glmdust$coefficients[2]*x)),add = T)


#for sex
byss=read.table("byss.txt",header = T)
malerisk=c()
maledust=c()
femalerisk=c()
femaledust=c()
byss=read.table("byss.txt",header = T)
for (a in 1:length(byss$sex)){
  if (byss$sex[a]==1){
    maleriska=byss$affected[a]/(byss$affected[a]+byss$not_affected[a])
    malerisk=c(malerisk,maleriska)
    maledust=c(maledust,byss$dust[a])
  } else{
    femaleriska=byss$affected[a]/(byss$affected[a]+byss$not_affected[a])
    femalerisk=c(femalerisk,femaleriska)
    femaledust=c(femaledust,byss$dust[a])
  }
}
par(mfrow=c(1,2))
plot(maledust,malerisk,main = "dust v.s risk for male",cex.lab=1.5,cex.main=1.5,ylim=c(0,0.4))
plot(femaledust,femalerisk,main = "dust v.s risk for female",cex.lab=1.5,cex.main=1.5,ylim = c(0,0.4))

#for smoker
byss=read.table("byss.txt",header = T)
nonsmokerisk=c()
nonsmokedust=c()
smokerisk=c()
smokedust=c()
for (a in 1:length(byss$smok)){
  if (byss$smok[a]==1){
    nonsmokeriska=byss$affected[a]/(byss$affected[a]+byss$not_affected[a])
    nonsmokerisk=c(nonsmokerisk,nonsmokeriska)
    nonsmokedust=c(nonsmokedust,byss$dust[a])
  } else{
    smokeriska=byss$affected[a]/(byss$affected[a]+byss$not_affected[a])
    smokerisk=c(smokerisk,smokeriska)
    smokedust=c(smokedust,byss$dust[a])
  }
}
par(mfrow=c(1,2))

plot(nonsmokedust,nonsmokerisk,main = "dust v.s risk for nonsmoker",cex.lab=1.5,cex.main=1.5,ylim=c(0,0.4))
plot(smokedust,smokerisk,main = "dust v.s risk for smoker",cex.lab=1.5,cex.main=1.5,ylim=c(0,0.4))


















