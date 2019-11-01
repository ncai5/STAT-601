

# Getting sample data
library(foreign)
mydata = read.dta("http://dss.princeton.edu/training/Panel101.dta")
# Running a logit model
logit = glm(y_bin ~ x1 + x2 + x3, family=binomial(link="logit"), data=mydata)
summary(logit)
# estimated coeff and 95% conf. interval
round(cbind(exp(logit$coefficients),exp(confint.default(logit))),3)

## Getting predicted probabilities holding all predictors or independent 
## variables to their means.
allmean = data.frame(x1=mean(mydata$x1),x2=mean(mydata$x2),x3=mean(mydata$x3))


pnew = predict.glm(logit, newdata=allmean, se.fit=T, type="link")

# predict.glm(logit, newdata=allmean, se.fit=T, type="response")
# When all predictor values are hold to their means,
# the estimated probability of y = 1 is 83%.

hat = pnew$fit; se.hat = pnew$se.fit
phat = exp(hat)/(1+exp(hat))
# c.i for log odds ratio
data.frame("lower bound"=hat-qnorm(0.975)*se.hat,"upper bound"=hat+qnorm(0.975)*se.hat) 
uphat = exp(hat-qnorm(0.975)*se.hat)/(1+exp(hat-qnorm(0.975)*se.hat))
lohat = exp(hat+qnorm(0.975)*se.hat)/(1+exp(hat+qnorm(0.975)*se.hat)) 
data.frame("lower bound"=uphat ,"upper bound"=lohat) # c.i for estimated prob

# LRT test for X2
logit.red = glm(y_bin~x1+x3, data=mydata, family=binomial("logit")) 
anova(logit.red, logit, test = "Chisq")


## two-way ANOVA (with no rep)
setwd("~/Downloads")
activity = read.csv('activity.csv');
head(activity)
activity$race = factor(activity$race)
activity$activity = factor(activity$activity)
activity.aov = aov(bmi~race+activity,data=activity)
summary(activity.aov)
