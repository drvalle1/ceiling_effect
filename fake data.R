rm(list=ls())
set.seed(101)

nobs=100
ceil1=6
floor1=0
mu.true=mu=5
tau2.true=tau2=3
sig2.true=sig2=0.3

#generate pre-scores
pr=pr.true=rnorm(nobs,mean=mu,sd=sqrt(tau2))
cond=pr>ceil1;  sum(cond); pr[cond]=ceil1
cond=pr<floor1; sum(cond); pr[cond]=floor1

#generate xmat
uniq=expand.grid(trat1=0:1,trat2=0:1,semester=0:1)
cond=uniq$trat1==1 & uniq$trat2==1
uniq=uniq[!cond,]
ind=sample(1:nrow(uniq),size=nobs,replace=T)
xmat.main=data.matrix(cbind(1,uniq[ind,]))
colnames(xmat.main)[1]='interc'

#generate xmat associated with pr
xmat.interact=cbind(1,xmat.main[,c('trat1','trat2')])
xmat.pr=xmat.interact*pr
xmat.pr.true=xmat.interact*pr.true

#parameters
betas.true=betas=c(0,0,-0.5,0.1)
gammas.true=gammas=c(1,0.2,0)

#generate data 
media=xmat.main%*%betas+xmat.pr.true%*%gammas
po=po.true=rnorm(nobs,mean=media,sd=sqrt(sig2))
cond=po>ceil1;  sum(cond); po[cond]=ceil1
cond=po<floor1; sum(cond); po[cond]=floor1

#final dataset
fim=data.frame(po=po,pr=pr,xmat.main[,-1])

#visualize these results
cores=rep('black',nrow(fim))
cond=fim$trat1==1; cores[cond]='red'
cond=fim$trat2==1; cores[cond]='blue'
rango=c(floor1,ceil1)
plot(fim$pr,fim$po,col=cores,xlim=rango,ylim=rango)
lines(rango,rango,col='grey',lwd=2)

#output results
setwd('U:\\GIT_models\\ceiling_effect')
write.csv(fim,'fake data.csv',row.names=F)
