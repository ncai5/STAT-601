biological_control <- c(37,42,12,32,97,68,14,29)
chemical_control <- c(41,45,18,106,107,171,12,30)
x=c(biological_control-chemical_control)
out = t.test(x,alternative = "two.sided", mu = 0,conf.level = .95)
out$statistic
out$p.value
out$conf.int

biological_control <- c(37,42,12,32,97,68,14,29)
chemical_control <- c(41,45,18,106,107,171,12,30)
out = t.test(x,y,alternative = "two.sided", mu = 0,conf.level = .95)
out$statistic
out$p.value
out$conf.int

#boxplot
biological_control <- c(37,42,12,32,97,68,14,29)
chemical_control <- c(41,45,18,106,107,171,12,30)
x=c(biological_control-chemical_control)
boxplot(x,main="Boxplot of difference of the two types of control ")