# want to repeat this procedure for different sample sizes. 
S = 100
par(mfrow = c(1,3)) # plot 4 figures 
for(n in c(10,40,160)){
  sample_mean=rep(NA,S)
  for(i in 1:S){
    sample_norm=rbinom(n,10,0.2)
    sample_mean[i]=mean(sample_norm)
  }
  if (n == 10){
    hist(sample_mean, xlim=c(0,5), ylim=c(0,50),xlab="sample mean",
         col=rgb(0,0,1,1/4), main="random sample(n=10) from B(10,0.2)", 
         breaks = seq(0,5,0.5))
  }
  if (n == 40){
    hist(sample_mean, xlim=c(0,5), ylim=c(0,50),xlab="sample mean",
         col=rgb(0,1,1,1/4), main="random sample(n=40) from B(10,0.2)", 
         breaks = seq(0,5,0.5))
  }
  if (n == 160){
    hist(sample_mean, xlim=c(0,5), ylim=c(0,50),xlab="sample mean",
         col=rgb(0,1/4,1,1/4), main="random sample(n=160) from B(10,0.2)", 
         breaks = seq(0,5,0.5))
  }
}