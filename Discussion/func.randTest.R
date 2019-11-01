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


