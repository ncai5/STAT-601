#4(f) Welch’s T test
x=c(1121,408,184,16,741,170,991,711,734,202,893,742,335,444)
y=c(1870,1324,1446,1325,1759,1652,1364,1515,1065)
out = t.test(y,x , alternative = "two.sided", mu = 0, paired = FALSE,conf.level = .95)
out$statistic
out$p.value
out$conf.int

#4(b)
x=c(1121,408,184,16,741,170,991,711,734,202,893,742,335,444)
y=c(1870,1324,1446,1325,1759,1652,1364,1515,1065)
out = t.test(x, y , alternative = "less", mu = 0, var.equal=TRUE,paired = FALSE,conf.level = .95)
out$statistic
out$p.value
#4(c)
x=c(1121,408,184,16,741,170,991,711,734,202,893,742,335,444)
y=c(1870,1324,1446,1325,1759,1652,1364,1515,1065)
out = t.test(y, x, alternative = "two.sided", mu = 0, paired = FALSE,var.equal=TRUE,conf.level = .95)
out$conf.int


#boxplot
x=c(1121,408,184,16,741,170,991,711,734,202,893,742,335,444)
y=c(1870,1324,1446,1325,1759,1652,1364,1515,1065)
par(mfrow=c(1,2))
boxplot(x,main="boxplot of infected buds")
boxplot(y,main="boxplot of healthy buds")


#5(j)
x=c(1121,408,184,16,741,170,991,711,734,202,893,742,335,444)
y=c(1870,1324,1446,1325,1759,1652,1364,1515,1065)
a=var(x)
b=var(y)
c=mean(x)
d=mean(y)
k=14/9
n1=(a+b/k)*(qnorm(0.975,0,1)+qnorm(0.8,0,1))^2/(c-d)^2
n2=(k*a+b)*(qnorm(0.975,0,1)+qnorm(0.8,0,1))^2/(c-d)^2

biological_control <- c(37,42,12,32,97,68,14,29)
chemical_control <- c(41,45,18,106,107,171,12,30)
par(mfrow=c(1,2))
stem(biological_control)     # stem-and-leaf display
hist(biological_control)     # histogram
stem(chemical_control)     # stem-and-leaf display
hist(chemical_control)     # histogram
biological_control <- c(37,42,12,32,97,68,14,29)
chemical_control <- c(41,45,18,106,107,171,12,30)
plot(x = biological_control, y = chemical_control) +
  geom_histogram(position = "identity", alpha = 0.4)
