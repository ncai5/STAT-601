
# Randomization Test ##########################################################################
# call functions "rand.test" from a source code "func.randTest.R"
#setwd("~/Disc4/") # set working directory
setwd("~/Downloads")
source("func.randTest.R")


# Independent Two Samples: "Weight Gain" data
dietA <- c(37.8, 27.5, 41.2, 26.5, 28.6)
dietB <- c(12.3, 14.3, 19.2, 4.0, 25.9)
set.seed(18); rand.test(dietA, dietB, paired=F)

# Paired Sample: "Blood Pressure" data
before <- c(90, 100, 92, 96, 96, 96, 92, 98, 102, 94, 94, 102, 94, 88, 104)
after <- c(88, 92, 82, 90, 78, 86, 88, 72, 84, 102, 94, 70, 94, 92, 94)
set.seed(18); rand.test(before, after, paired = T)



# Wilcoxon Test (Nonparametric) #######################################################
# Independent Two Samples (Wilcoxon rank sum test): "Weight Gain" data
wilcox.test(dietA, dietB)
wilcox.test(dietA, dietB, exact=TRUE)
wilcox.test(dietA, dietB, exact=FALSE)

# Paired Sample (Wilcoxon signed rank test): "Blood Pressure" data
wilcox.test(before, after, paired=TRUE)
wilcox.test(before, after, paired=TRUE, exact=TRUE)
wilcox.test(before, after, paired=TRUE, exact=FALSE)
