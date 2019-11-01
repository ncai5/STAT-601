par(mfrow=c(1,1))
kill.rate=c(3/50,5/49,19/47,19/38,24/49,35/50,2/50,14/49,20/50,27/50,41/50,40/50,28/50,37/50,46/50,48/50,48/50,50/50)
doses=c(2.00,2.64,3.48,4.59,6.06,8.00,2.00,2.64,3.48,4.59,6.06,8.00,2.00,2.64,3.48,4.59,6.06,8.00)
log(doses)
plot(log(doses),kill.rate,cex.lab=1.5)
#### 4 #####


insect=c(rep(0,6),rep(1,6),rep(2,6))

insc_type = factor(insect, levels = c(0, 1, 2),
                   labels = c("DDT", "BHC", "MIXED"))

dose=rep(c(2.00, 2.64, 3.48, 4.59, 6.06, 8.00),3)
killed=c(3,5,19,19,24,35,2,14,20,27,41,40,28,37,46,48,48,50)
unkilled=c(50,49,47,38,49,50,50,49,rep(50,10))-killed
data=data.frame("killed"=killed,"unkilled"=unkilled,"deposit"=dose,"type"=insc_type)

# making binary var 
y=c()
for(i in 1:length(killed)){
  yi=c(rep(1,killed[i]),rep(0,unkilled[i]))
  y=c(y,yi)
}

total=killed+unkilled
cumsum(total)
total=c(0,total)

XX = matrix(0,ncol = 2,nrow =sum(total))
X=as.matrix(data[,c(3,4)])
cum=cumsum(total)
for(i in 1:(length(cum)-1)){
  k=((cum[i]+1):(cum[i+1]))
  v=X[i,]
  for(j in 1:length(k)){
    XX[k[j],]=v
  }
}
XX=data.frame(XX)
XX$X1=as.numeric(as.character(XX$X1))
colnames(XX)=c("deposit","type")
head(XX)

XX.ddt=XX[XX$type=="DDT",]
doses.killrate1=glm(y[1:283]~log(XX.ddt$deposit),family=binomial(link="logit"))
doses.killrate1
abline(doses.killrate1)
XX.bhc=XX[XX$type=="BHC",]
doses.killrate2=glm(y[284:582]~log(XX.bhc$deposit),family=binomial(link="logit"))
doses.killrate2
abline(doses.killrate2)
XX.mixed=XX[XX$type=="MIXED",]
doses.killrate3=glm(y[583:882]~log(XX.mixed$deposit),family=binomial(link="logit"))
doses.killrate3
abline(doses.killrate3)

XX.ddt=XX[XX$type=="DDT",]
doses.killrate1=lm(y[1:283]~log(XX.ddt$deposit))
doses.killrate1
abline(doses.killrate1)
XX.bhc=XX[XX$type=="BHC",]
doses.killrate2=lm(y[284:299]~log(XX.bhc$deposit))
doses.killrate2
abline(doses.killrate2)
XX.mixed=XX[XX$type=="MIXED",]
doses.killrate3=lm(y[300:882]~log(XX.mixed$deposit))
doses.killrate3
abline(doses.killrate3)
