#n=10
sample_mean = c()
sample_variance = c()
for (i in 1:100)
{ sample<-rbinom(10,10,0.2)
sample_mean<-cbind(sample_mean,mean(sample))
sample_variance<-cbind(sample_variance,var(sample)
)
}
par(mfrow=c(1,2))
hist(sample_mean)
hist(sample_variance)

#n=40
sample_mean = c()
sample_variance = c()
for (i in 1:100)
{ sample<-rbinom(40,10,0.2)
sample_mean<-cbind(sample_mean,mean(sample))
sample_variance<-cbind(sample_variance,var(sample)
)
}
par(mfrow=c(1,2))
hist(sample_mean)
hist(sample_variance)

#n=160
sample_mean = c()
sample_variance = c()
for (i in 1:100)
{ sample<-rbinom(160,10,0.2)
sample_mean<-cbind(sample_mean,mean(sample))
sample_variance<-cbind(sample_variance,var(sample)
)
}
par(mfrow=c(1,2))
hist(sample_mean)
hist(sample_variance)