
relativeranking.fun<-function(mu, sigma){
#assumes larger values are better
#mu, sd are treatment specific outcomes
dimensions<-length(mu)


new.mu<-matrix(mu,nrow=dimensions,ncol=dimensions)
MD<-(new.mu-t(new.mu))#[upper.tri(dimensions2)]
new.sigma<-matrix(sigma,nrow=dimensions,ncol=dimensions)
sMD<-(sqrt(new.sigma^2+t(new.sigma^2)))#[upper.tri(dimensions2)]

z<-MD/sMD
p.better<-round(pnorm(z),3)

diag(p.better)<-0
Pscore<-apply(p.better,1,sum)/(dimensions-1)


###############

resampling<-mapply(rnorm,mean=mu,sd=sigma,n=10000)
#apply(resampling,2,mean)
#apply(resampling,2,sd)
rankings<-dimensions-t(apply(resampling, 1, rank))+1
MeanRank<-apply(rankings,2,mean)
MedianRank<-apply(rankings,2,median)
Pbest<-apply(apply(rankings,2,"==",1),2,sum)/10000
#SUCRAmr<-(dimensions-MeanRank)/(dimensions-1)
Prank<-c()
for(i in 1:dimensions){Prank<-cbind(Prank,table(factor(rankings[,i],levels=1:dimensions)))}
cumP<-apply(Prank,2,cumsum)/10000
SUCRA<-apply(cumP[1:(dimensions-1),],2,sum)/(dimensions-1)
print(list(Pscore=round(Pscore,3), SUCRA=round(SUCRA,3), MeanRank=round(MeanRank,3),MedianRank=MedianRank,Pbest=round(Pbest,3) ))

}




plotsNdensity.fun<-function(mu,sd, xi, maxYaxis=4,xaxisrange, cex.axis=1)
{
  #skewness parameter xi
  plot(xaxisrange,c(0,maxYaxis),type="n",xlab="Probability distributions",ylab="", cex.axis=cex.axis)
  howmany<-length(mu)
  for(i in 1:howmany){
    r<-rsnorm(1000,mean=mu[i], sd=sd[i], xi[i])
    x<-sort(r)
    y   <- dsnorm(x,mean=mu[i], sd=sd[i], xi[i])
    lines(x,y,lty=i, lwd=2)}
}


