Half_norm.plot=function(Effect,sig_index=NA,plot.name=NA){
  Effect=abs(Effect)
  n=length(Effect)
  Effect.sorted=Effect[order(Effect,decreasing = F)]
  Effect.names=gsub(":","",names(Effect.sorted))
  Effect.p = (c(1:n)-0.5)/n 
  Effect.q = qnorm(p=(0.5*Effect.p+0.5),mean=0,sd=1)
  Effect.0 = 0
  Effect.0.p = 0
  Effect.0.q = qnorm((0.5*Effect.0.p+0.5),mean=0,sd=1)
  Effect.50 = quantile(Effect.sorted,0.5)
  Effect.50.p = 0.5
  Effect.50.q = qnorm(0.5*Effect.50.p+0.5,mean=0,sd=1)
  line1 = line_ab(c(Effect.0,Effect.0.q),c(Effect.50,Effect.50.q))
  plot(Effect.sorted,Effect.q,pch=22,bg="darkgrey",
       main=plot.name,
       xaxt="n",xlim=c(0,max(Effect.sorted)),xlab="Effect",
       yaxt="n",ylim=c(qnorm(0.5*0.001+0.5,0,1),qnorm(0.5*0.999+0.5,0,1)),ylab="Half-normal % probability")
  abline(line1$intercept,line1$slope)
  axis(1,at=round(seq(0,max(Effect.sorted),length.out=5),2))
  axis(2,labels=c(0,20,40,60,70,80,85,90,95,97,99),
       at=qnorm(0.5*c(0,20,40,60,70,80,85,90,95,97,99)/100+0.5))
  text(Effect.sorted[sig_index],Effect.q[sig_index]+0.2,labels=Effect.names[sig_index],col="black")
}

Half_norm.plot(effect1,c(11:16),"half normal plot")

effect1=coefficients(Sicap.lm)[!is.na(coefficients(Sicap.lm))][-1]*2