#3
out = t.test(x=c(-1,2,1,4,-4,-5,-3,3,5,5,2,-1,3,3,-2,7,2,4,1,3),alternative = "greater", mu = 0, conf.level = .95)
out$statistic
out$p.value
out = t.test(x=c(-1,2,1,4,-4,-5,-3,3,5,5,2,-1,3,3,-2,7,2,4,1,3),alternative = "two.sided", mu = 0, conf.level = .90)
out$conf.int

#4(b)(c)
x=c(1121,408,184,16,741,170,991,711,734,202,893,742,335,444)
y=c(1870,1324,1446,1325,1759,1652,1364,1515,1065)
out = t.test(x, y , alternative = "less", mu = 0, paired = FALSE,conf.level = .95)
out$statistic
out$p.value

out = t.test(y, x, alternative = "two.sided", mu = 0, paired = FALSE,conf.level = .95)
out$conf.int
#4(d)
x=c(1121,408,184,16,741,170,991,711,734,202,893,742,335,444)
y=c(1870,1324,1446,1325,1759,1652,1364,1515,1065)
out = t.test(x, y , alternative = "two.sided", mu = 0, var.equal=TRUE,paired = FALSE,conf.level = .95)
out$statistic
out$p.value