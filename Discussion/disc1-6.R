rm(list = ls(all = TRUE))

# random generation for the normal / chi-squared / binomial distribution ######################
# "rnorm" is random number generation from the normal distribution.
rnorm(5,10,3) # sample n=5 observations from the normal distribution of mean=10 and sd=3
?rnorm        # "?rnorm" show a help. it is equivalent to "help(rnorm)"

set.seed(18)  # my seed number is 18
rnorm(5,10,3)

# "rchisq" is random number generation from the chi-squared distribution.
?rchisq
set.seed(18)
rchisq(5,2) # sample n=5 observations from the chi-squared distribution with df=2

# do for the binomial distribution by yourself


# descriptive statistics ######################################################################
set.seed(18)
sample_norm <- rnorm(100,10,3) # sample n=100 observations from N(10,3^2)
mean(sample_norm) # sample mean
var(sample_norm)  # sample variance
sd(sample_norm)   # sample sd

summary(sample_norm)
median(sample_norm)
IQR(sample_norm)
summary(sample_norm)[5] - summary(sample_norm)[2]
range(sample_norm)
diff(range(sample_norm))
summary(sample_norm)[6] - summary(sample_norm)[1]
quantile(sample_norm,  probs = c(0,0.01,0.10,0.25,0.50,0.75,1), type=1)

par(mfrow=c(1,1))
stem(sample_norm)     # stem-and-leaf display
hist(sample_norm)     # histogram

mydata = c(1,1,5,8,10,21,100)
summary(mydata)
IQR(mydata)
boxplot(mydata)  # box plot
boxplot(mydata,range = 1.5)  # show outlier(s) by 1.5*IQR rule
boxplot(mydata,range = 0)


# multiple figures in a single screen #########################################################
# "par(mfrow=c(nr,nc))" or "par(mfcol=c(nr,nc))" allow to draw nr-by-nc figures in the same page
par(mfrow=c(1,3))  # draw 1-by-3 figures
hist(sample_norm)
hist(sample_norm, freq=FALSE) # not frequency but density
hist(sample_norm, xlim=c(0,20), ylim=c(0,30)) # adjust xlim and ylim

# draw a pair of histograms or box plots ######################################################
set.seed(1818)
samp_a <- rnorm(100,10,3)
samp_b <- rnorm(100,15,3)

par(mfrow=c(1,3))
hist(samp_a)
hist(samp_b)
hist(samp_a)
hist(samp_b, add=T)

par(mfrow=c(1,3))
hist(samp_a, xlim=c(0,25))
hist(samp_b, xlim=c(0,25))
hist(samp_a, xlim=c(0,25))
hist(samp_b, xlim=c(0,25), add=T)

par(mfrow=c(1,3))
hist(samp_a, col=rgb(0,0,1,1/4), xlim=c(0,25))
hist(samp_b, col=rgb(1,0,0,1/4), xlim=c(0,25))
hist(samp_a, col=rgb(0,0,1,1/4), xlim=c(0,25), main="Histograms of A and B", xlab="Blue is A / Red is B")
hist(samp_b, col=rgb(1,0,0,1/4), xlim=c(0,25), add=T)

par(mfrow=c(1,2))
boxplot(samp_a)
boxplot(samp_b)

par(mfrow=c(1,1))
samp_ab <- c(samp_a, samp_b)
grp_ab  <- c(rep("a",100),rep("b",100))
boxplot(samp_ab~grp_ab)

#make a nice histogram
bark_urban = c(29,10,15,41,18,18,12,45,34,30,22,26,18)
bark_rural = c(40,47,38,59,45,52,57,50,50,49,50,43)

par(mfrow=c(1,2))
hist(bark_urban)
hist(bark_rural)

par(mfrow=c(1,2))
bins <- seq(10, 50, by=10)
hist(bark_urban,breaks=bins,ylim=c(0,7),main='urban',xlab='bark distance (m)') 
bins <- seq(30, 60, by=10)
hist(bark_rural,breaks=bins,ylim=c(0,7), main='rural',xlab='bark distance (m)')
