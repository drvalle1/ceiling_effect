rm(list=ls())
library('mvtnorm')
set.seed(1)

#get data
setwd('U:\\GIT_models\\ceiling_effect')
source('gibbs_ceiling_functions.R')
dat=read.csv('fake data.csv',as.is=T)
xmat=data.matrix(cbind(1,dat[,c('trat1','trat2')]))
colnames(xmat)[1]='interc'

#basic settings
ceil1=7
floor1=1
nobs=nrow(dat)

#initial values
betas=gammas=rep(0,ncol(xmat))
betas[1]=mean(dat$po)
sig2=var(dat$po)
mu=mean(dat$pr)
tau2=var(dat$pr)
pr.estim=dat$pr
po.estim=dat$po

#priors
invT=diag(1/10,ncol(xmat)*2)
a.prec=b.prec=0.1

#gibbs stuff
ngibbs=10000
store.betas=matrix(NA,ngibbs,length(betas))
store.gammas=matrix(NA,ngibbs,length(gammas))
store.others=matrix(NA,ngibbs,3)

for (i in 1:ngibbs){
  print(i)
  betas.gammas=sample.betas.gammas(xmat=xmat,pr.estim=pr.estim,sig2=sig2,
                                   po.estim=po.estim,invT=invT)
  betas=betas.gammas[1:ncol(xmat)]
  gammas=betas.gammas[(ncol(xmat)+1):(2*ncol(xmat))]
  
  sig2=sample.sig2(xmat=xmat,pr.estim=pr.estim,a.prec=a.prec,b.prec=b.prec,
                   po.estim=po.estim,betas.gammas=betas.gammas,nobs=nobs)
  mu=sample.mu(nobs=nobs,tau2=tau2,pr.estim=pr.estim)
  tau2=sample.tau2(nobs=nobs,a.prec=a.prec,b.prec=b.prec,pr.estim=pr.estim,
                   mu=mu)
  #parei aqui
  pr.estim=sample.pr.estim(xmat=xmat,sig2=sig2,gammas=gammas,tau2=tau2,
                           po.estim=po.estim,betas=betas,mu=mu,
                           pr=pr,ceil1=ceil1,floor1=floor1)
  po.estim=sample.po.estim(xmat=xmat,betas.gammas=betas.gammas,sig2=sig2,
                           po=po,ceil1=ceil1,floor1=floor1,pr.estim=pr.estim)
  
  #store results
  store.betas[i,]=betas
  store.gammas[i,]=gammas
  store.others[i,]=c(sig2,mu,tau2)
}