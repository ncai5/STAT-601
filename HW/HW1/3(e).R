# Find minimum n
for(n in 1:2000){
  prob=pnorm(qnorm(0.975,0,1)/sqrt(n),2,1/n)-pnorm(qnorm(0.025,0,1)/sqrt(n),2,1/n)
  if(prob <= 0.2){
    break
  }
  print(n)
}

n=9
pnorm(0.025,0,1)-pnorm(-0.025,0,1)

n=10
pnorm(0.025,0,1)-pnorm(-0.025,0,1)

a=pnorm(0.8,0,1)
b=pnorm(0.975,0,1)
c=(a+b)^2
