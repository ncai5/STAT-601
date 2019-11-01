par(mfrow=c(1,1))
Biological_control<-c(37,42,12,32,97,68,14,29)
Chemical_control<-c(41,45,18,106,107,171,12,30)
x=log(Biological_control)
qqnorm(x,main = "Q-Q Plot of transformed Biological_control")
qqline(x, col=2, lwd=2)

Biological_control<-c(37,42,12,32,97,68,14,29)
Chemical_control<-c(41,45,18,106,107,171,12,30)
y=log(Chemical_control)
qqnorm(y,main = "Q-Q Plot of transformed Chemical_control")
qqline(y, col=2, lwd=1)

#t-test
z=x-y
out = t.test(z,alternative = "two.sided", mu = 0, var.equal=TRUE,conf.level = .90)
out$statistic
out$p.value
out$conf.int