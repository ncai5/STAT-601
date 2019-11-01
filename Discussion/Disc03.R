###############################################################################################
# STAT 601 Discussion 03
###############################################################################################
rm(list = ls(all = TRUE))

# Function writing for simulation #############################################################
# HW1 Q3: histograms of the sample means and the sample variances (normal dist)
func.hist.norm <- function(n.simu, n.samp, mu, sig){
  par(mfcol=c(2,length(n.samp)))
  xlim_mean <- c(mu-sig, mu+sig); xlim_var <- c(0, 3*sig^2 )
  ylim_mean <- ylim_var <- c(0, n.simu*.55)
  for (n in n.samp) {
    sample_means <- rep(NA,n.simu)
    sample_vars  <- rep(NA,n.simu)
    for (i in 1:n.simu) {
      sample_norm <- rnorm(n,mean=mu,sd=sig)
      sample_means[i] <- mean(sample_norm)
      sample_vars[i]  <- var(sample_norm)
    }
    hist(sample_means, xlim=xlim_mean, ylim=ylim_mean,
         breaks=seq(xlim_mean[1], xlim_mean[2], length.out = 15), main = paste("n =",n))
    hist(sample_vars,  xlim=xlim_var,  ylim=ylim_var,
         breaks=seq(xlim_var[1], xlim_var[2], length.out = 15), main = paste("n =",n))
  }
  par(mfcol=c(1,1))
}

n.simu <- 100
n.samp <- c(10, 40, 160)
mu <- 4
sig <- 0.5
set.seed(181828); func.hist.norm(n.simu, n.samp, mu, sig)
set.seed(181828); func.hist.norm(n.simu=100, n.samp=c(10, 40, 160), mu=4, sig=0.5)
set.seed(181828); func.hist.norm(n.simu=50, n.samp=c(10, 30, 90), mu=10, sig=1)
set.seed(181828); func.hist.norm(n.simu=50, n.samp=c(10, 20, 40, 80), mu=10, sig=1)



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





# Randomization Test ##########################################################################
# call functions "rand.test" from a source code "func.randTest.R"
#setwd("~/Disc4/") # set working directory
source("func.randTest.R")

# Independent Two Samples: "Weight Gain" data
dietA <- c(37.8, 27.5, 41.2, 26.5, 28.6)
dietB <- c(12.3, 14.3, 19.2, 4.0, 25.9)
set.seed(18); rand.test(dietA, dietB, paired=F)

# Paired Sample: "Blood Pressure" data
before <- c(90, 100, 92, 96, 96, 96, 92, 98, 102, 94, 94, 102, 94, 88, 104)
after <- c(88, 92, 82, 90, 78, 86, 88, 72, 84, 102, 94, 70, 94, 92, 94)
set.seed(18); rand.test(before, after, paired = T)

# Wilcoxon Test ###############################################################################
# Independent Two Samples (Wilcoxon rank sum test): "Weight Gain" data
wilcox.test(dietA, dietB)
wilcox.test(dietA, dietB, exact=TRUE)
wilcox.test(dietA, dietB, exact=FALSE)

# Paired Sample (Wilcoxon signed rank test): "Blood Pressure" data
wilcox.test(before, after, paired=TRUE)
wilcox.test(before, after, paired=TRUE, exact=TRUE)
wilcox.test(before, after, paired=TRUE, exact=FALSE)
