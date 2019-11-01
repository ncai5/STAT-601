# Levene's test ###############################################################################
set.seed(18)
samp.norm1 <- rnorm(40, mean=10, sd=6)
samp.norm2 <- rnorm(10, mean=5, sd=3)
qqnorm(samp.norm1);   qqline(samp.norm1)
qqnorm(samp.norm2);   qqline(samp.norm2)

samp.norm   <- c(samp.norm1, samp.norm2)
samp.norm12 <- as.factor(c(rep(1,length(samp.norm1)),rep(2,length(samp.norm2))))
boxplot(samp.norm ~ samp.norm12)
var(samp.norm1); var(samp.norm2)


#install.packages("car") # install "car" package
library(car) # load "car" package to do Levene's test
leveneTest(samp.norm, samp.norm12)


# Welch's T test ##############################################################################
t.test(samp.norm1, samp.norm2, var.equal=TRUE)
t.test(samp.norm1, samp.norm2, var.equal=FALSE) # Welch T test

