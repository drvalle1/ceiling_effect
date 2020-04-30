tobit.pre.post=function(dat,ceil1,floor1,covs.main,var.betas,ngibbs){
  xmat=data.matrix(cbind(1,dat[,covs.main]))

  pr=dat$pr
  po=dat$po
  
  #basic settings
  ceil1=ceil1
  floor1=floor1
  nobs=nrow(dat)
  
  #initial values
  betas=rep(0,ncol(xmat))
  gammas=rep(0,ncol(xmat))
  betas[1]=mean(po)
  sig2=var(po)
  tau2=var(pr)
  pr.estim=pr
  po.estim=po
  mu=mean(pr)
  
  #priors
  invT=diag(1/var.betas,ncol(xmat)*2)
  a.prec=b.prec=0.1
  
  #gibbs stuff
  store.betas=matrix(NA,ngibbs,length(betas))
  store.gammas=matrix(NA,ngibbs,length(gammas))
  store.others=matrix(NA,ngibbs,3)
  
  for (i in 1:ngibbs){
    print(i)
    betas.gammas=sample.betas.gammas(xmat=xmat,
                                     pr.estim=pr.estim,sig2=sig2,
                                     po.estim=po.estim,invT=invT)
    betas=betas.gammas[1:ncol(xmat)]
    gammas=betas.gammas[(ncol(xmat)+1):(ncol(xmat)+ncol(xmat))]
    
    sig2=sample.sig2(xmat=xmat,pr.estim=pr.estim,
                     a.prec=a.prec,b.prec=b.prec,
                     po.estim=po.estim,betas.gammas=betas.gammas,nobs=nobs)
    mu=sample.mu(nobs=nobs,tau2=tau2,pr.estim=pr.estim)
    tau2=sample.tau2(nobs=nobs,a.prec=a.prec,b.prec=b.prec,pr.estim=pr.estim,mu=mu)
    
    pr.estim=sample.pr.estim(xmat=xmat,
                             sig2=sig2,gammas=gammas,tau2=tau2,
                             po.estim=po.estim,betas=betas,mu=mu,
                             pr=pr,ceil1=ceil1,floor1=floor1)

    po.estim=sample.po.estim(xmat=xmat,
                             betas.gammas=betas.gammas,sig2=sig2,
                             po=po,ceil1=ceil1,floor1=floor1,pr.estim=pr.estim)

    #store results
    store.betas[i,]=betas
    store.gammas[i,]=gammas
    store.others[i,]=c(sig2,mu,tau2)
  }
  
  #correct parameters to remove centering effect
  list(betas=cbind(store.betas,store.gammas),
       sig2=store.others[,1],
       mu=store.others[,2],
       tau2=store.others[,3])
}
  
