# Randomization Test ##########################################################################
# call functions "rand.test" from a source code "func.randTest.R"

rand.test <- function(y1, y2, paired=NULL) {
  nsims <- 10000
  
  if (paired==FALSE || is.null(paired)) {
    x  <- c(y1,y2)
    gp <- as.factor(c(rep("a",length(y1)), rep("b",length(y2))))
    labs <- unique(gp)
    obsmeandiff <- mean(x[gp==labs[1]]) - mean(x[gp==labs[2]])
    meandiffvec <- NULL
    for( i in 1:nsims){
      newgp <- sample(gp, size = length(gp))
      newmeandiff <- mean(x[newgp==labs[1]]) - mean(x[newgp==labs[2]])
      meandiffvec <- c(meandiffvec, newmeandiff)
    }
    pl <- sum(meandiffvec >= obsmeandiff)
    pg <- sum(meandiffvec <= obsmeandiff)
    pval <-  ifelse(pl < pg, 2 * pl/nsims, 2 * pg/nsims)
    
    hhh <- hist(meandiffvec, plot=FALSE)
    hist(meandiffvec, xlab="Differences of means",
         main=paste("Independent Sample Randomization Test, p=", pval),
         probability=TRUE, xlim=range(c(hhh$breaks, obsmeandiff)))
    abline(v=obsmeandiff, col="red")
    mtext(paste(round(obsmeandiff, digits=4)), side=1, at=obsmeandiff)
  } 
  
  else {
    d <- y1 - y2
    ld <- length(d)
    obsmeand <- mean(d)
    meandvec <- NULL
    for( i in 1:nsims){
      rsigns <- 2 * rbinom(ld, 1, .5) - 1
      newd <- rsigns * d
      newmeand <- mean(newd)
      meandvec <- c(meandvec, newmeand)
    }
    pl <- length(meandvec[meandvec >= obsmeand])
    pg <- length(meandvec[meandvec <= obsmeand])
    pval <-  ifelse(pl < pg, 2 * pl/nsims, 2 * pg/nsims)
    hhh <- hist(meandvec, plot=FALSE)
    hist(meandvec, xlab="Mean of differences", 
         main=paste("Paired Sample Randomization Test, p=", pval),
         probability=TRUE, xlim=range(c(hhh$breaks, obsmeand)))
    abline(v=obsmeand, col="red")
    mtext(paste(round(obsmeand, digits=4)), side=1, at=obsmeand)
  }
}

# Independent Two Samples: "2-year-old seedlings" data
x=c(1121,408,184,16,741,170,991,711,734,202,893,742,335,444)
y=c(1870,1324,1446,1325,1759,1652,1364,1515,1065)
set.seed(18);
rand.test(x, y, paired=F)

# Wilcoxon Test (Nonparametric) #######################################################
# Independent Two Samples (Wilcoxon rank sum test): "2-year-old seedlings" data
x=c(1121,408,184,16,741,170,991,711,734,202,893,742,335,444)
y=c(1870,1324,1446,1325,1759,1652,1364,1515,1065)
wilcox.test(x, y)
wilcox.test(x, y, exact=TRUE)
wilcox.test(x, y, exact=FALSE)

# Paired Sample: "biological control and chemical control" data
biological_control <- c(37,42,12,32,97,68,14,29)
chemical_control <- c(41,45,18,106,107,171,12,30)
set.seed(18); 
rand.test(biological_control,chemical_control, paired = T)

# Paired Sample (Wilcoxon signed rank test): "biological control and chemical control" data
biological_control <- c(37,42,12,32,97,68,14,29)
chemical_control <- c(41,45,18,106,107,171,12,30)
wilcox.test(biological_control, chemical_control, paired=TRUE)
wilcox.test(biological_control, chemical_control, paired=TRUE, exact=TRUE)
wilcox.test(biological_control, chemical_control, paired=TRUE, exact=FALSE)