tnorm <- function(n,lo,hi,mu,sig){   #generates truncated normal variates based on cumulative normal distribution
  #normal truncated lo and hi
  
  if(length(lo) == 1 & length(mu) > 1)lo <- rep(lo,length(mu))
  if(length(hi) == 1 & length(mu) > 1)hi <- rep(hi,length(mu))
  
  q1 <- pnorm(lo,mu,sig) #cumulative distribution
  q2 <- pnorm(hi,mu,sig) #cumulative distribution
  
  z <- runif(n,q1,q2)
  z <- qnorm(z,mu,sig)
  z[z == -Inf]  <- lo[z == -Inf]
  z[z == Inf]   <- hi[z == Inf]
  z
}
#-----------------------------------
sample.betas.gammas=function(xmat.main,xmat.interact,pr.estim,sig2,po.estim,invT){
  xmat1=cbind(xmat.main,xmat.interact*pr.estim)
  xtx=t(xmat1)%*%xmat1
  prec=(1/sig2)*xtx+invT
  var1=solve(prec)
  pmedia=(1/sig2)*t(xmat1)%*%po.estim
  t(rmvnorm(1,var1%*%pmedia,var1))
}
#--------------------------------
sample.sig2=function(xmat.main,xmat.interact,pr.estim,a.prec,b.prec,po.estim,
                     betas.gammas,nobs){
  a1=(nobs/2)+a.prec
  xmat1=cbind(xmat.main,xmat.interact*pr.estim)
  media=xmat1%*%betas.gammas
  err=po.estim-media
  ssq=t(err)%*%err
  b1=(ssq/2)+b.prec
  1/rgamma(1,a1,b1)
}
#--------------------------------
sample.mu=function(nobs,tau2,pr.estim){
  prec=(nobs/tau2)+(1/100)
  var1=1/prec
  pmedia=(1/tau2)*sum(pr.estim)
  rnorm(1,mean=var1*pmedia,sd=sqrt(var1))
}
#--------------------------------
sample.tau2=function(nobs,a.prec,b.prec,pr.estim,mu){
  a1=(nobs/2)+a.prec
  err2=(pr.estim-mu)^2
  b1=b.prec+sum(err2)/2
  1/rgamma(1,a1,b1)
}
#--------------------------------
sample.pr.estim=function(xmat.main,xmat.interact,sig2,gammas,tau2,po.estim,betas,mu,
                         pr,ceil1,floor1){
  xgamma2=(xmat.interact%*%gammas)^2
  prec=(1/sig2)*xgamma2+(1/tau2)
  var1=1/prec
  p2=(1/tau2)*mu
  p1=(1/sig2)*(po.estim-xmat.main%*%betas)*(xmat.interact%*%gammas)
  pmedia=p1+p2
  media=pmedia*var1
  
  #input pr.estim
  pr.estim=pr
  cond=pr==ceil1; 
  pr.estim[cond]=tnorm(sum(cond),lo=ceil1,hi=Inf  ,mu=media[cond],sig=sqrt(var1[cond]))
  cond=pr==floor1; 
  pr.estim[cond]=tnorm(sum(cond),lo=-Inf,hi=floor1,mu=media[cond],sig=sqrt(var1[cond]))
  pr.estim
}
#------------------------
sample.po.estim=function(xmat.main,xmat.interact,betas.gammas,sig2,po,ceil1,floor1,
                         pr.estim){
  xmat1=cbind(xmat.main,xmat.interact*pr.estim)
  media=xmat1%*%betas.gammas
  
  #input po.estim
  po.estim=po
  cond=po==ceil1;  po.estim[cond]=tnorm(sum(cond),lo=ceil1,hi=Inf,  mu=media[cond],sig=sqrt(sig2))
  cond=po==floor1; po.estim[cond]=tnorm(sum(cond),lo=-Inf,hi=floor1,mu=media[cond],sig=sqrt(sig2))  
  po.estim
}