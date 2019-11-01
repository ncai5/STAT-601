a<-read.csv("height worldwide 1996.csv",header = T)
x=c(a$difference)
out = t.test(x, alternative = "greater", mu = 0, var.equal=TRUE,conf.level = .95)
out$statistic
out$p.value
out = t.test(x, alternative = "two.sided", mu = 0, var.equal=TRUE,conf.level = .95)
out$conf.int