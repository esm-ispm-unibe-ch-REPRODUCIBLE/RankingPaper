
relativeranking.fun<-function(mu, sigma,xi,lowbest=T){
  #assumes low values are best
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
 if(missing(xi)){xi=rep(1,length(sd))}
  resampling<-mapply(rsnorm,mean=mu,sd=sigma,xi=xi,n=100000)
  #apply(resampling,2,mean)
  #apply(resampling,2,sd)
  if(lowbest==F){rankings<-dimensions-t(apply(resampling, 1, rank))+1}
  else{rankings<-t(apply(resampling, 1, rank))}
  MeanRank<-apply(rankings,2,mean)
  MedianRank<-apply(rankings,2,median)
  Pbest<-apply(apply(rankings,2,"==",1),2,sum)/100000
  #SUCRAmr<-(dimensions-MeanRank)/(dimensions-1)
  Prank<-c()
  for(i in 1:dimensions){Prank<-cbind(Prank,table(factor(rankings[,i],levels=1:dimensions)))}
  cumP<-apply(Prank,2,cumsum)/100000
  SUCRA<-apply(cumP[1:(dimensions-1),],2,sum)/(dimensions-1)
  
  print(list(Pscore=round(Pscore,3), SUCRA=round(SUCRA,3), MeanRank=round(MeanRank,3),MedianRank=MedianRank,Pbest=round(Pbest,3) ))
  
}


plotNdensity.fun<-function(mu,sd,xi, maxYaxis=4,xaxisrange, cex.axis=1, cols,ltys)
{

  if(missing(xi)){ xi=rep(1,length(mu))}
  if(missing(ltys)){ ltys=rep(1,length(mu))}
  if(missing(cols)){ cols=rep(1,length(mu))}
  plot(xaxisrange,c(0,maxYaxis),type="n",xlab="Probability distributions",ylab="", cex.axis=cex.axis)
  howmany<-length(mu)
  for(i in 1:howmany){
    r<-rsnorm(10000,mean=mu[i], sd=sd[i],xi=xi[i])
    x<-sort(r)
    y   <- dsnorm(x,mean=mu[i], sd=sd[i], xi=xi[i])
    lines(x,y,lty=ltys[i], lwd=2, col=cols[i])}
}



