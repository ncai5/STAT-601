# mvrnorm
library(mixtools)  #for ellipse
N <- 10 # Number of random samples
set.seed(123)
# Target parameters for univariate normal distributions
rho <- -0.6
mu1 <- 1; s1 <- 2
mu2 <- 1; s2 <- 8

# Parameters for bivariate normal distribution
mu <- c(mu1,mu2) # Mean 
sigma <- matrix(c(s1^2, s1*s2*rho, s1*s2*rho, s2^2),2) # Covariance matrix

# Function to draw ellipse for bivariate normal data
ellipse_bvn <- function(bvn, alpha){
  Xbar <- apply(bvn,2,mean)
  S <- cov(bvn)
  ellipse(Xbar, S, alpha = alpha, col="red")
}
#1
library(MASS)
bvn1 <- mvrnorm(N, mu = mu, Sigma = sigma ) # from MASS package
colnames(bvn1) <- c("bvn1_X1","bvn1_X2")
bvn1
#2
M <- t(chol(sigma))
# M %*% t(M)
Z <- matrix(rnorm(2*N),2,N) # 2 rows, N/2 columns
bvn2 <- t(M %*% Z) + matrix(rep(mu,N), byrow=TRUE,ncol=2)
colnames(bvn2) <- c("bvn2_X1","bvn2_X2")
bvn2

par(mfrow=c(1,1))
bvn <- list(bvn1,bvn2)
plot(bvn1, xlab="X1",ylab="X2",main= "All Samples")
for(i in 2:2){
  points(bvn[[i]],col=i)
}
for(i in 1:2){
  item <- paste("bvn",i,sep="")
  plot(bvn[[i]],xlab="X1",ylab="X2",main=item, col=i)
  ellipse_bvn(bvn[[i]],.5)
  ellipse_bvn(bvn[[i]],.05)
}







library(MASS)
library(mvtnorm)
M = matrix(c(1,0.5,0.5,0.5,1,0.5,0.5,0.5,1),nrow=3)
dmvnorm(1:3,mu=1:3,Sigma=M,log=TRUE)
dmvnorm(matrix(1:6,nrow=2),mu=1:3,Sigma=M,log=TRUE)
dmvnorm(matrix(1:6,nrow=2),mu=matrix(1:6,nrow=2),Sigma=M,log=TRUE)

# 3
rbvn<-function (n, m1, s1, m2, s2, rho) 
{
  X1 <- rnorm(n, mu1, s1)
  X2 <- rnorm(n, mu2 + (s2/s1) * rho * 
                (X1 - mu1), sqrt((1 - rho^2)*s2^2))
  cbind(X1, X2)
}
bvn3 <- rbvn(N,mu1,s1,mu2,s2,rho)
colnames(bvn3) <- c("bvn3_X1","bvn3_X2")


#
dmvnorm(x=c(0,0))
dmvnorm(x=c(0,0), mean=c(1,1))
sigma <- matrix(c(4,2,2,3), ncol=2)
x <- rmvnorm(n=500, mean=c(1,2), sigma=sigma)
colMeans(x)
var(x)
x <- rmvnorm(n=500, mean=c(1,2), sigma=sigma, method="chol")
colMeans(x)
var(x)
plot(x)


library(MASS)
set.seed(4776)
mvrnorm(2, mu=c(0,0), Sigma=rbind(c(1, 0),c(0, 1)))