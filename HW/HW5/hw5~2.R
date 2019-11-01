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

ddt=c(rep(1,283),rep(0,299),rep(0,300))
bhc=c(rep(0,283),rep(1,299),rep(0,300))
X=data.frame("ldose"=log(XX$deposit),"ddt"=ddt,"bhc"=bhc)

glm=glm(y~X$ldose+X$ddt+X$bhc,family=binomial(link="logit"))
glm(y~X$ldose+X$ddt+X$bhc-1,family=binomial(link="logit"))

confint(glm,level = 0.9)

glm(y~X$ldose+X$ddt+X$bhc,family=binomial(link="cloglog"))



##4(b)
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


par(mfrow=c(1,1))
kill.rate=c(3/50,5/49,19/47,19/38,24/49,35/50,2/50,14/49,20/50,27/50,41/50,40/50,28/50,37/50,46/50,48/50,48/50,50/50)
doses=c(2.00,2.64,3.48,4.59,6.06,8.00,2.00,2.64,3.48,4.59,6.06,8.00,2.00,2.64,3.48,4.59,6.06,8.00)
log(doses)
doses.killrate=lm(kill.rate~doses)
plot(log(doses),kill.rate,cex.lab=1.5)
glm=glm(y~log(XX$deposit)+XX$type,family=binomial(link="logit"))
lines(log(XX$deposit)[1:283],fitted(glm)[1:283])
lines(log(XX$deposit)[284:582],fitted(glm)[284:582])
lines(log(XX$deposit)[583:882],fitted(glm)[583:882])



x1=c(rep(1,6),rep(0,6),rep(0,6))
x2=c(rep(0,6),rep(1,6),rep(0,6))
dose=rep(c(2.00, 2.64, 3.48, 4.59, 6.06, 8.00),3)
killrate=c(3/50,5/49,19/47,19/38,24/49,35/50,2/50,14/49,20/50,27/50,41/50,40/50,28/50,37/50,46/50,48/50,48/50,50/50)
reglm=lm(killrate~dose+x1+x2)



